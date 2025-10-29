{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly.VCL.Types
Description : AST types for Fastly VCL (Varnish Configuration Language)
Copyright   : (c) 2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module contains the Abstract Syntax Tree (AST) data types for representing
Fastly VCL code. VCL is used to configure Fastly's edge caching behavior.
-}

module Network.Fastly.VCL.Types
  ( -- * Top-level VCL Document
    VCL(..)
  , TopLevel(..)

    -- * Subroutines
  , Subroutine(..)
  , SubroutineName(..)

    -- * Statements
  , Statement(..)

    -- * Expressions
  , Expr(..)
  , BinOp(..)
  , UnOp(..)

    -- * Types
  , VCLType(..)
  , Literal(..)

    -- * Identifiers and Variables
  , Identifier(..)
  , Variable(..)

    -- * ACLs and Backends
  , ACL(..)
  , ACLEntry(..)
  , Backend(..)
  , BackendProperty(..)

    -- * Director
  , Director(..)
  , DirectorType(..)

    -- * Table
  , Table(..)
  , TableProperty(..)
  ) where

import Data.Text (Text)
import GHC.Generics
import Data.Int (Int64)

-- ---------------------------------------------------------------------------
-- Top-level VCL Document
-- ---------------------------------------------------------------------------

-- | A complete VCL document consisting of top-level declarations.
newtype VCL = VCL [TopLevel]
  deriving (Show, Eq, Generic)

-- | Top-level declarations in a VCL file.
data TopLevel
  = TopLevelSubroutine Subroutine
  -- ^ A subroutine definition
  | TopLevelACL ACL
  -- ^ An ACL (Access Control List) definition
  | TopLevelBackend Backend
  -- ^ A backend server definition
  | TopLevelDirector Director
  -- ^ A director (load balancer) definition
  | TopLevelTable Table
  -- ^ A table (edge dictionary) definition
  | TopLevelInclude Text
  -- ^ Include another VCL file
  | TopLevelImport Text
  -- ^ Import a VCL module
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Subroutines
-- ---------------------------------------------------------------------------

-- | A VCL subroutine (sub) definition.
data Subroutine = Subroutine
  { subroutineName :: SubroutineName
  -- ^ Name of the subroutine
  , subroutineBody :: [Statement]
  -- ^ Statements in the subroutine body
  } deriving (Show, Eq, Generic)

-- | Subroutine names, including predefined ones.
data SubroutineName
  = VclRecv        -- ^ vcl_recv - handles incoming requests
  | VclHash        -- ^ vcl_hash - determines cache key
  | VclHit         -- ^ vcl_hit - handles cache hits
  | VclMiss        -- ^ vcl_miss - handles cache misses
  | VclPass        -- ^ vcl_pass - handles pass mode
  | VclFetch       -- ^ vcl_fetch - handles backend responses
  | VclError       -- ^ vcl_error - handles error responses
  | VclDeliver     -- ^ vcl_deliver - handles delivery to client
  | VclLog         -- ^ vcl_log - handles logging
  | CustomSub Text -- ^ User-defined subroutine
  deriving (Show, Eq, Ord, Generic)

-- ---------------------------------------------------------------------------
-- Statements
-- ---------------------------------------------------------------------------

-- | VCL statements that can appear in subroutine bodies.
data Statement
  = Set Variable Expr
  -- ^ Set a variable to a value: set var.foo = expr;
  | Unset Variable
  -- ^ Unset a variable: unset var.foo;
  | Declare Identifier VCLType (Maybe Expr)
  -- ^ Declare a local variable: declare local var.foo STRING;
  | If Expr [Statement] [(Expr, [Statement])] (Maybe [Statement])
  -- ^ if (expr) { stmts } elsif (expr) { stmts } else { stmts }
  | Return (Maybe Identifier)
  -- ^ return or return(action);
  | Call SubroutineName
  -- ^ call subroutine;
  | Log Expr
  -- ^ log expr;
  | AddHeader Identifier Expr
  -- ^ add header = value;
  | RemoveHeader Identifier
  -- ^ remove header;
  | Error Int64 (Maybe Text)
  -- ^ error 404 or error 404 "Not Found";
  | Restart
  -- ^ restart;
  | Synthetic Expr
  -- ^ synthetic expr;
  | SyntheticBase64 Expr
  -- ^ synthetic.base64 expr;
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

-- | VCL expressions.
data Expr
  = Lit Literal
  -- ^ Literal value
  | Var Variable
  -- ^ Variable reference
  | BinOp BinOp Expr Expr
  -- ^ Binary operation: expr op expr
  | UnOp UnOp Expr
  -- ^ Unary operation: op expr
  | FunctionCall Identifier [Expr]
  -- ^ Function call: func(args)
  deriving (Show, Eq, Generic)

-- | Binary operators in VCL.
data BinOp
  = Add      -- ^ +
  | Sub      -- ^ -
  | Mul      -- ^ *
  | Div      -- ^ /
  | Mod      -- ^ %
  | Eq       -- ^ ==
  | Ne       -- ^ !=
  | Lt       -- ^ <
  | Le       -- ^ <=
  | Gt       -- ^ >
  | Ge       -- ^ >=
  | And      -- ^ &&
  | Or       -- ^ ||
  | Match    -- ^ ~ (regex match)
  | NotMatch -- ^ !~ (regex not match)
  | Concat   -- ^ String concatenation (implicit or explicit)
  deriving (Show, Eq, Generic)

-- | Unary operators in VCL.
data UnOp
  = Not -- ^ !
  | Neg -- ^ - (negation)
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Types and Literals
-- ---------------------------------------------------------------------------

-- | VCL data types.
data VCLType
  = TString  -- ^ STRING
  | TInteger -- ^ INTEGER
  | TFloat   -- ^ FLOAT
  | TBool    -- ^ BOOL
  | TTime    -- ^ TIME
  | TRTime   -- ^ RTIME (relative time/duration)
  | TIP      -- ^ IP
  | TACL     -- ^ ACL
  | TBackend -- ^ BACKEND
  deriving (Show, Eq, Generic)

-- | Literal values in VCL.
data Literal
  = LString Text
  -- ^ String literal: "hello"
  | LInteger Int64
  -- ^ Integer literal: 42
  | LFloat Double
  -- ^ Float literal: 3.14
  | LBool Bool
  -- ^ Boolean literal: true or false
  | LDuration Text
  -- ^ Duration literal: 1h, 30m, 60s, etc.
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Identifiers and Variables
-- ---------------------------------------------------------------------------

-- | An identifier (simple name).
newtype Identifier = Identifier Text
  deriving (Show, Eq, Ord, Generic)

-- | A variable, which can be a simple identifier or a dotted path.
--
-- Examples: req.http.Host, beresp.status, var.my_local
data Variable = Variable [Text]
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- ACLs
-- ---------------------------------------------------------------------------

-- | An Access Control List definition.
data ACL = ACL
  { aclName    :: Identifier
  -- ^ Name of the ACL
  , aclEntries :: [ACLEntry]
  -- ^ List of ACL entries
  } deriving (Show, Eq, Generic)

-- | An entry in an ACL.
data ACLEntry = ACLEntry
  { aclEntryNegated :: Bool
  -- ^ Whether this is a negation (!)/deny entry
  , aclEntryIP      :: Text
  -- ^ IP address or CIDR range
  } deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Backends
-- ---------------------------------------------------------------------------

-- | A backend server definition.
data Backend = Backend
  { backendName       :: Identifier
  -- ^ Backend name
  , backendProperties :: [BackendProperty]
  -- ^ Backend configuration properties
  } deriving (Show, Eq, Generic)

-- | Backend configuration properties.
data BackendProperty
  = BackendHost Text
  -- ^ .host = "example.com"
  | BackendPort Int64
  -- ^ .port = "443"
  | BackendConnectTimeout Text
  -- ^ .connect_timeout = 1s
  | BackendFirstByteTimeout Text
  -- ^ .first_byte_timeout = 15s
  | BackendBetweenBytesTimeout Text
  -- ^ .between_bytes_timeout = 10s
  | BackendSSL Bool
  -- ^ .ssl = true
  | BackendSSLCertHostname Text
  -- ^ .ssl_cert_hostname = "example.com"
  | BackendSSLSNIHostname Text
  -- ^ .ssl_sni_hostname = "example.com"
  | BackendMaxConnections Int64
  -- ^ .max_connections = 200
  | BackendProbe Text
  -- ^ .probe = "health_check"
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Directors
-- ---------------------------------------------------------------------------

-- | A director (load balancer) definition.
data Director = Director
  { directorName    :: Identifier
  -- ^ Director name
  , directorType    :: DirectorType
  -- ^ Type of director
  , directorBackends :: [Identifier]
  -- ^ List of backend names in this director
  } deriving (Show, Eq, Generic)

-- | Director types (load balancing strategies).
data DirectorType
  = Random    -- ^ Random selection
  | RoundRobin -- ^ Round-robin
  | Hash      -- ^ Consistent hashing
  | Client    -- ^ Client-based
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Tables (Edge Dictionaries)
-- ---------------------------------------------------------------------------

-- | A table (edge dictionary) definition.
data Table = Table
  { tableName       :: Identifier
  -- ^ Table name
  , tableProperties :: [TableProperty]
  -- ^ Table properties
  } deriving (Show, Eq, Generic)

-- | Table configuration properties.
data TableProperty
  = TableType VCLType
  -- ^ Type of values in this table
  | TableDefault Literal
  -- ^ Default value if key not found
  deriving (Show, Eq, Generic)
