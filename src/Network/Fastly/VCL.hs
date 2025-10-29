{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly.VCL
Description : Parsing, rendering, and generating Fastly VCL code
Copyright   : (c) 2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module provides comprehensive support for working with Fastly VCL
(Varnish Configuration Language) code. It includes:

* Parsing VCL from text
* Rendering VCL AST to formatted text
* Generating VCL programmatically using the AST
* Validating VCL for semantic correctness

= Usage Examples

== Parsing VCL

>>> import Network.Fastly.VCL
>>> let vclCode = "sub vcl_recv { set req.http.Host = \"example.com\"; }"
>>> case parseVCL vclCode of
...   Left err -> print err
...   Right vcl -> putStrLn "Parsed successfully!"

== Generating VCL

>>> import Network.Fastly.VCL
>>> let vcl = VCL [TopLevelSubroutine $ Subroutine VclRecv [Set (Variable ["req", "http", "Host"]) (Lit $ LString "example.com")]]
>>> putStrLn $ renderVCL vcl

== Round-trip parsing and rendering

>>> case parseVCL vclCode of
...   Left err -> print err
...   Right vcl -> putStrLn $ renderVCL vcl

== Validating VCL

>>> case parseVCL vclCode of
...   Left err -> print err
...   Right vcl -> case validateVCL vcl of
...     Left errors -> mapM_ print errors
...     Right _ -> putStrLn "VCL is valid!"
-}

module Network.Fastly.VCL
  ( -- * Main VCL type
    VCL(..)
  , TopLevel(..)

    -- * Parsing
  , parseVCL
  , parseVCLFile
  , parseExpr
  , parseStatement
  , ParseError

    -- * Validation
  , validateVCL
  , validateTopLevel
  , validateSubroutine
  , validateStatement
  , validateExpr
  , ValidationError(..)
  , ValidationResult
  , ValidationWarning(..)
  , SubroutineContext(..)

    -- * Rendering
  , renderVCL
  , renderVCLWith
  , RenderConfig(..)
  , defaultRenderConfig

    -- * Pretty-printing (for display)
  , prettyVCL
  , prettyTopLevel
  , prettySubroutine
  , prettyStatement
  , prettyExpr

    -- * AST Types
  , module Network.Fastly.VCL.Types

    -- * Building VCL programmatically
  , subroutine
  , setVar
  , unsetVar
  , ifStmt
  , ifElse
  , returnStmt
  , returnWith
  , callSub
  , logStmt

    -- * Building expressions
  , stringLit
  , intLit
  , floatLit
  , boolLit
  , durationLit
  , var
  , (.==.)
  , (./=.)
  , (.<.)
  , (.<=.)
  , (.>.)
  , (.>=.)
  , (.&&.)
  , (.||.)
  , (.~.)
  , (.!~.)
  , (.+.)
  , (.-.)
  , (.*.)
  , (./.)
  , (.%.)
  , notExpr
  , negExpr
  , funcCall
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Network.Fastly.VCL.Types
import Network.Fastly.VCL.Parser
import Network.Fastly.VCL.Pretty
import Network.Fastly.VCL.Validation

-- ---------------------------------------------------------------------------
-- Builder Functions for Subroutines and Statements
-- ---------------------------------------------------------------------------

-- | Create a subroutine.
subroutine :: SubroutineName -> [Statement] -> TopLevel
subroutine name body = TopLevelSubroutine $ Subroutine name body

-- | Create a set statement.
setVar :: [Text] -> Expr -> Statement
setVar varPath expr = Set (Variable varPath) expr

-- | Create an unset statement.
unsetVar :: [Text] -> Statement
unsetVar varPath = Unset (Variable varPath)

-- | Create a simple if statement (no elsif or else).
ifStmt :: Expr -> [Statement] -> Statement
ifStmt cond thenStmts = If cond thenStmts [] Nothing

-- | Create an if-else statement.
ifElse :: Expr -> [Statement] -> [Statement] -> Statement
ifElse cond thenStmts elseStmts = If cond thenStmts [] (Just elseStmts)

-- | Create a return statement without action.
returnStmt :: Statement
returnStmt = Return Nothing

-- | Create a return statement with action.
returnWith :: Text -> Statement
returnWith action = Return (Just $ Identifier action)

-- | Create a call statement.
callSub :: SubroutineName -> Statement
callSub = Call

-- | Create a log statement.
logStmt :: Expr -> Statement
logStmt = Log

-- ---------------------------------------------------------------------------
-- Builder Functions for Expressions
-- ---------------------------------------------------------------------------

-- | Create a string literal.
stringLit :: Text -> Expr
stringLit = Lit . LString

-- | Create an integer literal.
intLit :: Int -> Expr
intLit = Lit . LInteger . fromIntegral

-- | Create a float literal.
floatLit :: Double -> Expr
floatLit = Lit . LFloat

-- | Create a boolean literal.
boolLit :: Bool -> Expr
boolLit = Lit . LBool

-- | Create a duration literal.
durationLit :: Text -> Expr
durationLit = Lit . LDuration

-- | Create a variable reference.
var :: [Text] -> Expr
var = Var . Variable

-- | Equal operator.
(.==.) :: Expr -> Expr -> Expr
(.==.) = BinOp Eq
infixl 4 .==.

-- | Not equal operator.
(./=.) :: Expr -> Expr -> Expr
(./=.) = BinOp Ne
infixl 4 ./=.

-- | Less than operator.
(.<.) :: Expr -> Expr -> Expr
(.<.) = BinOp Lt
infixl 4 .<.

-- | Less than or equal operator.
(.<=.) :: Expr -> Expr -> Expr
(.<=.) = BinOp Le
infixl 4 .<=.

-- | Greater than operator.
(.>.) :: Expr -> Expr -> Expr
(.>.) = BinOp Gt
infixl 4 .>.

-- | Greater than or equal operator.
(.>=.) :: Expr -> Expr -> Expr
(.>=.) = BinOp Ge
infixl 4 .>=.

-- | Logical AND operator.
(.&&.) :: Expr -> Expr -> Expr
(.&&.) = BinOp And
infixl 3 .&&.

-- | Logical OR operator.
(.||.) :: Expr -> Expr -> Expr
(.||.) = BinOp Or
infixl 2 .||.

-- | Regex match operator.
(.~.) :: Expr -> Expr -> Expr
(.~.) = BinOp Match
infixl 4 .~.

-- | Regex not match operator.
(.!~.) :: Expr -> Expr -> Expr
(.!~.) = BinOp NotMatch
infixl 4 .!~.

-- | Addition operator.
(.+.) :: Expr -> Expr -> Expr
(.+.) = BinOp Add
infixl 6 .+.

-- | Subtraction operator.
(.-.) :: Expr -> Expr -> Expr
(.-.) = BinOp Sub
infixl 6 .-.

-- | Multiplication operator.
(.*.) :: Expr -> Expr -> Expr
(.*.) = BinOp Mul
infixl 7 .*.

-- | Division operator.
(./.) :: Expr -> Expr -> Expr
(./.) = BinOp Div
infixl 7 ./.

-- | Modulo operator.
(.%.) :: Expr -> Expr -> Expr
(.%.) = BinOp Mod
infixl 7 .%.

-- | Logical NOT operator.
notExpr :: Expr -> Expr
notExpr = UnOp Not

-- | Negation operator.
negExpr :: Expr -> Expr
negExpr = UnOp Neg

-- | Function call.
funcCall :: Text -> [Expr] -> Expr
funcCall name args = FunctionCall (Identifier name) args
