{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly.VCL.Pretty
Description : Pretty-printer for Fastly VCL (Varnish Configuration Language)
Copyright   : (c) 2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module provides pretty-printing functionality for VCL AST using the
prettyprinter library.
-}

module Network.Fastly.VCL.Pretty
  ( -- * Pretty-printing functions
    renderVCL
  , renderVCLWith
  , prettyVCL
  , prettyTopLevel
  , prettySubroutine
  , prettyStatement
  , prettyExpr

    -- * Configuration
  , RenderConfig(..)
  , defaultRenderConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Text

import Network.Fastly.VCL.Types

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for rendering VCL.
data RenderConfig = RenderConfig
  { renderWidth :: Int
  -- ^ Maximum line width (default: 80)
  , renderIndent :: Int
  -- ^ Number of spaces per indentation level (default: 2)
  } deriving (Show, Eq)

-- | Default render configuration.
defaultRenderConfig :: RenderConfig
defaultRenderConfig = RenderConfig
  { renderWidth = 80
  , renderIndent = 2
  }

-- ---------------------------------------------------------------------------
-- Rendering Functions
-- ---------------------------------------------------------------------------

-- | Render VCL to text with default configuration.
renderVCL :: VCL -> Text
renderVCL = renderVCLWith defaultRenderConfig

-- | Render VCL to text with custom configuration.
renderVCLWith :: RenderConfig -> VCL -> Text
renderVCLWith config vcl =
  let opts = defaultLayoutOptions { layoutPageWidth = AvailablePerLine (renderWidth config) 1.0 }
  in renderStrict $ layoutPretty opts $ prettyVCL vcl

-- ---------------------------------------------------------------------------
-- Pretty-printing VCL
-- ---------------------------------------------------------------------------

-- | Pretty-print a VCL document.
prettyVCL :: VCL -> Doc ann
prettyVCL (VCL tops) = vsep (map prettyTopLevel tops) <> line

-- | Pretty-print a top-level declaration.
prettyTopLevel :: TopLevel -> Doc ann
prettyTopLevel (TopLevelSubroutine sub) = prettySubroutine sub
prettyTopLevel (TopLevelACL acl) = prettyACL acl
prettyTopLevel (TopLevelBackend backend) = prettyBackend backend
prettyTopLevel (TopLevelDirector dir) = prettyDirector dir
prettyTopLevel (TopLevelTable tbl) = prettyTable tbl
prettyTopLevel (TopLevelInclude path) = "include" <+> dquotes (pretty path) <> semi
prettyTopLevel (TopLevelImport mod) = "import" <+> pretty mod <> semi

-- ---------------------------------------------------------------------------
-- Subroutines
-- ---------------------------------------------------------------------------

-- | Pretty-print a subroutine.
prettySubroutine :: Subroutine -> Doc ann
prettySubroutine (Subroutine name body) =
  "sub" <+> prettySubroutineName name <+> lbrace
  <> line
  <> indent 2 (vsep (map prettyStatement body))
  <> line
  <> rbrace
  <> line

-- | Pretty-print a subroutine name.
prettySubroutineName :: SubroutineName -> Doc ann
prettySubroutineName VclRecv = "vcl_recv"
prettySubroutineName VclHash = "vcl_hash"
prettySubroutineName VclHit = "vcl_hit"
prettySubroutineName VclMiss = "vcl_miss"
prettySubroutineName VclPass = "vcl_pass"
prettySubroutineName VclFetch = "vcl_fetch"
prettySubroutineName VclError = "vcl_error"
prettySubroutineName VclDeliver = "vcl_deliver"
prettySubroutineName VclLog = "vcl_log"
prettySubroutineName (CustomSub name) = pretty name

-- ---------------------------------------------------------------------------
-- Statements
-- ---------------------------------------------------------------------------

-- | Pretty-print a statement.
prettyStatement :: Statement -> Doc ann
prettyStatement (Set var expr) =
  "set" <+> prettyVariable var <+> equals <+> prettyExpr expr <> semi

prettyStatement (Unset var) =
  "unset" <+> prettyVariable var <> semi

prettyStatement (Declare ident typ init) =
  "declare local" <+> prettyIdentifier ident <+> prettyVCLType typ
  <> maybe emptyDoc (\e -> space <> equals <+> prettyExpr e) init <> semi

prettyStatement (If cond thenStmts elsifs elseStmts) =
  "if" <+> parens (prettyExpr cond) <+> lbrace
  <> line
  <> indent 2 (vsep (map prettyStatement thenStmts))
  <> line
  <> rbrace
  <> vsep (map prettyElsif elsifs)
  <> maybe emptyDoc prettyElse elseStmts

prettyStatement (Return Nothing) = "return" <> semi

prettyStatement (Return (Just action)) =
  "return" <> parens (prettyIdentifier action) <> semi

prettyStatement (Call name) =
  "call" <+> prettySubroutineName name <> semi

prettyStatement (Log expr) =
  "log" <+> prettyExpr expr <> semi

prettyStatement (AddHeader ident expr) =
  "add" <+> prettyIdentifier ident <+> equals <+> prettyExpr expr <> semi

prettyStatement (RemoveHeader ident) =
  "remove" <+> prettyIdentifier ident <> semi

prettyStatement (Error code msg) =
  "error" <+> pretty code
  <> maybe emptyDoc (\m -> space <> dquotes (pretty m)) msg <> semi

prettyStatement Restart = "restart" <> semi

prettyStatement (Synthetic expr) =
  "synthetic" <+> prettyExpr expr <> semi

prettyStatement (SyntheticBase64 expr) =
  "synthetic.base64" <+> prettyExpr expr <> semi

-- | Pretty-print an elsif clause.
prettyElsif :: (Expr, [Statement]) -> Doc ann
prettyElsif (cond, stmts) =
  space <> "elsif" <+> parens (prettyExpr cond) <+> lbrace
  <> line
  <> indent 2 (vsep (map prettyStatement stmts))
  <> line
  <> rbrace

-- | Pretty-print an else clause.
prettyElse :: [Statement] -> Doc ann
prettyElse stmts =
  space <> "else" <+> lbrace
  <> line
  <> indent 2 (vsep (map prettyStatement stmts))
  <> line
  <> rbrace

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

-- | Pretty-print an expression.
prettyExpr :: Expr -> Doc ann
prettyExpr (Lit lit) = prettyLiteral lit
prettyExpr (Var var) = prettyVariable var
prettyExpr (BinOp op e1 e2) = prettyExprWithParens e1 <+> prettyBinOp op <+> prettyExprWithParens e2
prettyExpr (UnOp op e) = prettyUnOp op <> prettyExprWithParens e
prettyExpr (FunctionCall name args) =
  prettyIdentifier name <> parens (align $ sep $ punctuate comma $ map prettyExpr args)

-- | Pretty-print an expression, adding parentheses if needed.
prettyExprWithParens :: Expr -> Doc ann
prettyExprWithParens e@(BinOp {}) = parens (prettyExpr e)
prettyExprWithParens e@(UnOp {}) = parens (prettyExpr e)
prettyExprWithParens e = prettyExpr e

-- | Pretty-print a binary operator.
prettyBinOp :: BinOp -> Doc ann
prettyBinOp Add = "+"
prettyBinOp Sub = "-"
prettyBinOp Mul = "*"
prettyBinOp Div = "/"
prettyBinOp Mod = "%"
prettyBinOp Eq = "=="
prettyBinOp Ne = "!="
prettyBinOp Lt = "<"
prettyBinOp Le = "<="
prettyBinOp Gt = ">"
prettyBinOp Ge = ">="
prettyBinOp And = "&&"
prettyBinOp Or = "||"
prettyBinOp Match = "~"
prettyBinOp NotMatch = "!~"
prettyBinOp Concat = "+"

-- | Pretty-print a unary operator.
prettyUnOp :: UnOp -> Doc ann
prettyUnOp Not = "!"
prettyUnOp Neg = "-"

-- | Pretty-print a literal.
prettyLiteral :: Literal -> Doc ann
prettyLiteral (LString s) = dquotes (pretty s)
prettyLiteral (LInteger i) = pretty i
prettyLiteral (LFloat f) = pretty f
prettyLiteral (LBool True) = "true"
prettyLiteral (LBool False) = "false"
prettyLiteral (LDuration d) = pretty d

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Pretty-print a VCL type.
prettyVCLType :: VCLType -> Doc ann
prettyVCLType TString = "STRING"
prettyVCLType TInteger = "INTEGER"
prettyVCLType TFloat = "FLOAT"
prettyVCLType TBool = "BOOL"
prettyVCLType TTime = "TIME"
prettyVCLType TRTime = "RTIME"
prettyVCLType TIP = "IP"
prettyVCLType TACL = "ACL"
prettyVCLType TBackend = "BACKEND"

-- ---------------------------------------------------------------------------
-- Identifiers and Variables
-- ---------------------------------------------------------------------------

-- | Pretty-print an identifier.
prettyIdentifier :: Identifier -> Doc ann
prettyIdentifier (Identifier name) = pretty name

-- | Pretty-print a variable.
prettyVariable :: Variable -> Doc ann
prettyVariable (Variable parts) = hcat (punctuate dot (map pretty parts))

-- ---------------------------------------------------------------------------
-- ACLs
-- ---------------------------------------------------------------------------

-- | Pretty-print an ACL.
prettyACL :: ACL -> Doc ann
prettyACL (ACL name entries) =
  "acl" <+> prettyIdentifier name <+> lbrace
  <> line
  <> indent 2 (vsep (map prettyACLEntry entries))
  <> line
  <> rbrace
  <> line

-- | Pretty-print an ACL entry.
prettyACLEntry :: ACLEntry -> Doc ann
prettyACLEntry (ACLEntry negated ip) =
  (if negated then "!" else emptyDoc) <> dquotes (pretty ip) <> semi

-- ---------------------------------------------------------------------------
-- Backends
-- ---------------------------------------------------------------------------

-- | Pretty-print a backend.
prettyBackend :: Backend -> Doc ann
prettyBackend (Backend name props) =
  "backend" <+> prettyIdentifier name <+> lbrace
  <> line
  <> indent 2 (vsep (map prettyBackendProperty props))
  <> line
  <> rbrace
  <> line

-- | Pretty-print a backend property.
prettyBackendProperty :: BackendProperty -> Doc ann
prettyBackendProperty (BackendHost host) =
  dot <> "host" <+> equals <+> dquotes (pretty host) <> semi
prettyBackendProperty (BackendPort port) =
  dot <> "port" <+> equals <+> dquotes (pretty port) <> semi
prettyBackendProperty (BackendConnectTimeout timeout) =
  dot <> "connect_timeout" <+> equals <+> pretty timeout <> semi
prettyBackendProperty (BackendFirstByteTimeout timeout) =
  dot <> "first_byte_timeout" <+> equals <+> pretty timeout <> semi
prettyBackendProperty (BackendBetweenBytesTimeout timeout) =
  dot <> "between_bytes_timeout" <+> equals <+> pretty timeout <> semi
prettyBackendProperty (BackendSSL ssl) =
  dot <> "ssl" <+> equals <+> if ssl then "true" else "false" <> semi
prettyBackendProperty (BackendSSLCertHostname hostname) =
  dot <> "ssl_cert_hostname" <+> equals <+> dquotes (pretty hostname) <> semi
prettyBackendProperty (BackendSSLSNIHostname hostname) =
  dot <> "ssl_sni_hostname" <+> equals <+> dquotes (pretty hostname) <> semi
prettyBackendProperty (BackendMaxConnections max) =
  dot <> "max_connections" <+> equals <+> pretty max <> semi
prettyBackendProperty (BackendProbe probe) =
  dot <> "probe" <+> equals <+> dquotes (pretty probe) <> semi

-- ---------------------------------------------------------------------------
-- Directors
-- ---------------------------------------------------------------------------

-- | Pretty-print a director.
prettyDirector :: Director -> Doc ann
prettyDirector (Director name dirType backends) =
  "director" <+> prettyIdentifier name <+> prettyDirectorType dirType <+> lbrace
  <> line
  <> indent 2 (vsep (map prettyBackendRef backends))
  <> line
  <> rbrace
  <> line

-- | Pretty-print a backend reference in a director.
prettyBackendRef :: Identifier -> Doc ann
prettyBackendRef ident = dot <> "backend" <+> equals <+> prettyIdentifier ident <> semi

-- | Pretty-print a director type.
prettyDirectorType :: DirectorType -> Doc ann
prettyDirectorType Random = "random"
prettyDirectorType RoundRobin = "round-robin"
prettyDirectorType Hash = "hash"
prettyDirectorType Client = "client"

-- ---------------------------------------------------------------------------
-- Tables
-- ---------------------------------------------------------------------------

-- | Pretty-print a table.
prettyTable :: Table -> Doc ann
prettyTable (Table name props) =
  "table" <+> prettyIdentifier name <+> lbrace
  <> line
  <> indent 2 (vsep (map prettyTableProperty props))
  <> line
  <> rbrace
  <> line

-- | Pretty-print a table property.
prettyTableProperty :: TableProperty -> Doc ann
prettyTableProperty (TableType typ) =
  "type" <+> equals <+> prettyVCLType typ <> semi
prettyTableProperty (TableDefault lit) =
  "default" <+> equals <+> prettyLiteral lit <> semi
