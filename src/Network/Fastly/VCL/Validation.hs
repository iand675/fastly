{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Network.Fastly.VCL.Validation
Description : Semantic validation for Fastly VCL
Copyright   : (c) 2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module provides semantic validation for VCL code to catch errors
that are syntactically valid but semantically incorrect.
-}

module Network.Fastly.VCL.Validation
  ( -- * Validation
    validateVCL
  , validateTopLevel
  , validateSubroutine
  , validateStatement
  , validateExpr

    -- * Validation errors
  , ValidationError(..)
  , ValidationResult
  , ValidationWarning(..)

    -- * Context
  , ValidationContext(..)
  , SubroutineContext(..)
  , emptyContext
  ) where

import Control.Monad (foldM, when, unless, forM_)
import Control.Monad.State
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Network.Fastly.VCL.Types

-- ---------------------------------------------------------------------------
-- Validation Types
-- ---------------------------------------------------------------------------

-- | Validation errors that indicate semantic problems.
data ValidationError
  = UndefinedVariable Variable
  -- ^ Variable used but not defined in this context
  | InvalidVariableContext Variable SubroutineContext
  -- ^ Variable used in wrong subroutine context
  | ReadOnlyVariable Variable
  -- ^ Attempted to set a read-only variable
  | TypeMismatch VCLType VCLType Text
  -- ^ Type mismatch: expected, got, context
  | UndefinedSubroutine SubroutineName
  -- ^ Called subroutine doesn't exist
  | UndefinedBackend Identifier
  -- ^ Referenced backend doesn't exist
  | UndefinedACL Identifier
  -- ^ Referenced ACL doesn't exist
  | InvalidReturnAction Identifier SubroutineContext
  -- ^ Invalid return action for this subroutine
  | DuplicateDefinition Text
  -- ^ Duplicate backend, ACL, or subroutine name
  | InvalidOperation Text
  -- ^ Operation not allowed in this context
  | UnreachableCode
  -- ^ Code after return/error/restart
  | MissingReturn SubroutineName
  -- ^ Subroutine missing return statement
  deriving (Show, Eq)

-- | Validation warnings for potential issues.
data ValidationWarning
  = UnusedVariable Identifier
  -- ^ Variable declared but never used
  | ShadowedVariable Identifier
  -- ^ Variable shadows another declaration
  | RedundantSet Variable
  -- ^ Setting variable to same value twice
  deriving (Show, Eq)

-- | Result of validation.
type ValidationResult a = Either [ValidationError] a

-- | Subroutine context determines what operations are valid.
data SubroutineContext
  = RecvContext    -- ^ vcl_recv
  | HashContext    -- ^ vcl_hash
  | HitContext     -- ^ vcl_hit
  | MissContext    -- ^ vcl_miss
  | PassContext    -- ^ vcl_pass
  | FetchContext   -- ^ vcl_fetch
  | ErrorContext   -- ^ vcl_error
  | DeliverContext -- ^ vcl_deliver
  | LogContext     -- ^ vcl_log
  | CustomContext  -- ^ Custom subroutine
  deriving (Show, Eq)

-- | Validation context tracking defined entities and current scope.
data ValidationContext = ValidationContext
  { ctxSubroutines :: Set SubroutineName
  -- ^ Defined subroutines
  , ctxBackends :: Set Identifier
  -- ^ Defined backends
  , ctxACLs :: Set Identifier
  -- ^ Defined ACLs
  , ctxLocalVars :: Map Identifier VCLType
  -- ^ Local variables in scope
  , ctxCurrentSub :: Maybe SubroutineContext
  -- ^ Current subroutine context
  , ctxErrors :: [ValidationError]
  -- ^ Accumulated errors
  , ctxWarnings :: [ValidationWarning]
  -- ^ Accumulated warnings
  } deriving (Show, Eq)

-- | Empty validation context.
emptyContext :: ValidationContext
emptyContext = ValidationContext
  { ctxSubroutines = Set.empty
  , ctxBackends = Set.empty
  , ctxACLs = Set.empty
  , ctxLocalVars = Map.empty
  , ctxCurrentSub = Nothing
  , ctxErrors = []
  , ctxWarnings = []
  }

type Validator a = State ValidationContext a

-- ---------------------------------------------------------------------------
-- Main Validation Functions
-- ---------------------------------------------------------------------------

-- | Validate a complete VCL document.
validateVCL :: VCL -> ValidationResult VCL
validateVCL vcl@(VCL tops) = do
  let (_, ctx) = runState (validateVCLM tops) emptyContext
  if null (ctxErrors ctx)
    then Right vcl
    else Left (ctxErrors ctx)

-- | Internal validation with state.
validateVCLM :: [TopLevel] -> Validator ()
validateVCLM tops = do
  -- First pass: collect all definitions
  forM_ tops collectDefinitions
  -- Second pass: validate each top-level item
  forM_ tops validateTopLevelM

-- | Collect definitions from top-level items.
collectDefinitions :: TopLevel -> Validator ()
collectDefinitions (TopLevelSubroutine (Subroutine name _)) = do
  ctx <- get
  when (name `Set.member` ctxSubroutines ctx) $
    addError $ DuplicateDefinition $ "subroutine " <> showSubName name
  modify $ \c -> c { ctxSubroutines = Set.insert name (ctxSubroutines c) }

collectDefinitions (TopLevelBackend (Backend name _)) = do
  ctx <- get
  when (name `Set.member` ctxBackends ctx) $
    addError $ DuplicateDefinition $ "backend " <> showIdent name
  modify $ \c -> c { ctxBackends = Set.insert name (ctxBackends c) }

collectDefinitions (TopLevelACL (ACL name _)) = do
  ctx <- get
  when (name `Set.member` ctxACLs ctx) $
    addError $ DuplicateDefinition $ "ACL " <> showIdent name
  modify $ \c -> c { ctxACLs = Set.insert name (ctxACLs c) }

collectDefinitions _ = return ()

-- | Validate a top-level declaration.
validateTopLevel :: TopLevel -> ValidationResult TopLevel
validateTopLevel top = do
  let (_, ctx) = runState (validateTopLevelM top) emptyContext
  if null (ctxErrors ctx)
    then Right top
    else Left (ctxErrors ctx)

-- | Internal top-level validation.
validateTopLevelM :: TopLevel -> Validator ()
validateTopLevelM (TopLevelSubroutine sub) = validateSubroutineM sub
validateTopLevelM (TopLevelBackend backend) = validateBackendM backend
validateTopLevelM (TopLevelACL acl) = validateACLM acl
validateTopLevelM (TopLevelDirector dir) = validateDirectorM dir
validateTopLevelM (TopLevelTable _) = return () -- Tables are mostly declarative
validateTopLevelM (TopLevelInclude _) = return ()
validateTopLevelM (TopLevelImport _) = return ()

-- ---------------------------------------------------------------------------
-- Subroutine Validation
-- ---------------------------------------------------------------------------

-- | Validate a subroutine.
validateSubroutine :: SubroutineContext -> Subroutine -> ValidationResult Subroutine
validateSubroutine subCtx sub = do
  let ctx = emptyContext { ctxCurrentSub = Just subCtx }
  let (_, finalCtx) = runState (validateSubroutineM sub) ctx
  if null (ctxErrors finalCtx)
    then Right sub
    else Left (ctxErrors finalCtx)

-- | Internal subroutine validation.
validateSubroutineM :: Subroutine -> Validator ()
validateSubroutineM (Subroutine name body) = do
  let subCtx = subroutineNameToContext name
  modify $ \c -> c { ctxCurrentSub = Just subCtx }

  -- Validate all statements
  hasReturn <- validateStatementsM body

  -- Check if subroutine needs a return
  when (not hasReturn && isPredefinedSub name) $
    addError $ MissingReturn name

  -- Reset context
  modify $ \c -> c { ctxCurrentSub = Nothing, ctxLocalVars = Map.empty }

-- | Validate statements and check if there's a return.
validateStatementsM :: [Statement] -> Validator Bool
validateStatementsM [] = return False
validateStatementsM (stmt:rest) = do
  terminates <- validateStatementM stmt
  when (terminates && not (null rest)) $
    addError UnreachableCode
  if terminates
    then return True
    else validateStatementsM rest

-- ---------------------------------------------------------------------------
-- Statement Validation
-- ---------------------------------------------------------------------------

-- | Validate a statement.
validateStatement :: SubroutineContext -> Statement -> ValidationResult Statement
validateStatement subCtx stmt = do
  let ctx = emptyContext { ctxCurrentSub = Just subCtx }
  let (_, finalCtx) = runState (validateStatementM stmt) ctx
  if null (ctxErrors finalCtx)
    then Right stmt
    else Left (ctxErrors finalCtx)

-- | Internal statement validation. Returns True if statement terminates execution.
validateStatementM :: Statement -> Validator Bool
validateStatementM (Set var expr) = do
  -- Check if variable is writable
  checkVariableWritable var

  -- Validate the expression
  validateExprM expr

  return False

validateStatementM (Unset var) = do
  checkVariableWritable var
  return False

validateStatementM (Declare ident typ maybeExpr) = do
  -- Check for duplicate declaration
  ctx <- get
  when (ident `Map.member` ctxLocalVars ctx) $
    addWarning $ ShadowedVariable ident

  -- Add to local vars
  modify $ \c -> c { ctxLocalVars = Map.insert ident typ (ctxLocalVars c) }

  -- Validate initializer if present
  case maybeExpr of
    Just expr -> do
      exprType <- inferExprType expr
      when (exprType /= typ && exprType /= TString) $  -- Allow implicit conversions from string
        addError $ TypeMismatch typ exprType "variable initialization"
      validateExprM expr
    Nothing -> return ()

  return False

validateStatementM (If cond thenStmts elsifs elseStmts) = do
  -- Validate condition
  condType <- inferExprType cond
  when (condType /= TBool) $
    addError $ TypeMismatch TBool condType "if condition"
  validateExprM cond

  -- Validate branches
  thenReturns <- validateStatementsM thenStmts
  elsifsReturn <- and <$> mapM validateElsifM elsifs
  elseReturns <- case elseStmts of
    Just stmts -> validateStatementsM stmts
    Nothing -> return False

  -- All branches return only if all have returns
  return (thenReturns && elsifsReturn && elseReturns)

validateStatementM (Return maybeAction) = do
  ctx <- get
  case (maybeAction, ctxCurrentSub ctx) of
    (Just action, Just subCtx) ->
      unless (isValidReturnAction action subCtx) $
        addError $ InvalidReturnAction action subCtx
    _ -> return ()
  return True

validateStatementM (Call name) = do
  ctx <- get
  unless (name `Set.member` ctxSubroutines ctx) $
    addError $ UndefinedSubroutine name
  return False

validateStatementM (Log expr) = do
  validateExprM expr
  return False

validateStatementM (AddHeader ident expr) = do
  validateExprM expr
  return False

validateStatementM (RemoveHeader _) = return False

validateStatementM (Error _ _) = return True

validateStatementM Restart = return True

validateStatementM (Synthetic expr) = do
  exprType <- inferExprType expr
  when (exprType /= TString) $
    addError $ TypeMismatch TString exprType "synthetic"
  validateExprM expr
  return False

validateStatementM (SyntheticBase64 expr) = do
  exprType <- inferExprType expr
  when (exprType /= TString) $
    addError $ TypeMismatch TString exprType "synthetic.base64"
  validateExprM expr
  return False

-- | Validate elsif clause.
validateElsifM :: (Expr, [Statement]) -> Validator Bool
validateElsifM (cond, stmts) = do
  condType <- inferExprType cond
  when (condType /= TBool) $
    addError $ TypeMismatch TBool condType "elsif condition"
  validateExprM cond
  validateStatementsM stmts

-- ---------------------------------------------------------------------------
-- Expression Validation
-- ---------------------------------------------------------------------------

-- | Validate an expression.
validateExpr :: Expr -> ValidationResult Expr
validateExpr expr = do
  let (_, ctx) = runState (validateExprM expr) emptyContext
  if null (ctxErrors ctx)
    then Right expr
    else Left (ctxErrors ctx)

-- | Internal expression validation.
validateExprM :: Expr -> Validator ()
validateExprM (Lit _) = return ()

validateExprM (Var var) = do
  checkVariableReadable var

validateExprM (BinOp op e1 e2) = do
  t1 <- inferExprType e1
  t2 <- inferExprType e2
  validateBinaryOp op t1 t2
  validateExprM e1
  validateExprM e2

validateExprM (UnOp op e) = do
  t <- inferExprType e
  validateUnaryOp op t
  validateExprM e

validateExprM (FunctionCall _ args) = do
  -- Validate all arguments
  mapM_ validateExprM args

-- | Validate binary operation type compatibility.
validateBinaryOp :: BinOp -> VCLType -> VCLType -> Validator ()
validateBinaryOp op t1 t2 = case op of
  Add | t1 == TInteger && t2 == TInteger -> return ()
      | t1 == TFloat && t2 == TFloat -> return ()
      | t1 == TString || t2 == TString -> return ()  -- String concatenation
      | otherwise -> addError $ TypeMismatch t1 t2 "addition"

  Sub | t1 == TInteger && t2 == TInteger -> return ()
      | t1 == TFloat && t2 == TFloat -> return ()
      | otherwise -> addError $ TypeMismatch t1 t2 "subtraction"

  Mul | t1 == TInteger && t2 == TInteger -> return ()
      | t1 == TFloat && t2 == TFloat -> return ()
      | otherwise -> addError $ TypeMismatch t1 t2 "multiplication"

  Div | t1 == TInteger && t2 == TInteger -> return ()
      | t1 == TFloat && t2 == TFloat -> return ()
      | otherwise -> addError $ TypeMismatch t1 t2 "division"

  Mod | t1 == TInteger && t2 == TInteger -> return ()
      | otherwise -> addError $ TypeMismatch t1 t2 "modulo"

  Eq -> when (t1 /= t2 && t1 /= TString && t2 /= TString) $
          addError $ TypeMismatch t1 t2 "equality comparison"

  Ne -> when (t1 /= t2 && t1 /= TString && t2 /= TString) $
          addError $ TypeMismatch t1 t2 "inequality comparison"

  Lt -> validateComparison t1 t2
  Le -> validateComparison t1 t2
  Gt -> validateComparison t1 t2
  Ge -> validateComparison t1 t2

  And | t1 == TBool && t2 == TBool -> return ()
      | otherwise -> addError $ TypeMismatch TBool t1 "logical AND"

  Or | t1 == TBool && t2 == TBool -> return ()
     | otherwise -> addError $ TypeMismatch TBool t1 "logical OR"

  Match -> when (t1 /= TString || t2 /= TString) $
             addError $ TypeMismatch TString t1 "regex match"

  NotMatch -> when (t1 /= TString || t2 /= TString) $
                addError $ TypeMismatch TString t1 "regex not match"

  Concat -> return ()  -- String concatenation is flexible

-- | Validate comparison operation.
validateComparison :: VCLType -> VCLType -> Validator ()
validateComparison t1 t2
  | t1 == t2 && t1 `elem` [TInteger, TFloat, TString, TTime, TRTime] = return ()
  | otherwise = addError $ TypeMismatch t1 t2 "comparison"

-- | Validate unary operation type compatibility.
validateUnaryOp :: UnOp -> VCLType -> Validator ()
validateUnaryOp Not t
  | t == TBool = return ()
  | otherwise = addError $ TypeMismatch TBool t "logical NOT"

validateUnaryOp Neg t
  | t `elem` [TInteger, TFloat] = return ()
  | otherwise = addError $ TypeMismatch TInteger t "negation"

-- ---------------------------------------------------------------------------
-- Type Inference
-- ---------------------------------------------------------------------------

-- | Infer the type of an expression.
inferExprType :: Expr -> Validator VCLType
inferExprType (Lit lit) = return $ literalType lit

inferExprType (Var var) = variableType var

inferExprType (BinOp op e1 e2) = do
  t1 <- inferExprType e1
  t2 <- inferExprType e2
  return $ binOpResultType op t1 t2

inferExprType (UnOp op e) = do
  t <- inferExprType e
  return $ unOpResultType op t

inferExprType (FunctionCall _ _) = return TString  -- Conservative: assume string

-- | Get the type of a literal.
literalType :: Literal -> VCLType
literalType (LString _) = TString
literalType (LInteger _) = TInteger
literalType (LFloat _) = TFloat
literalType (LBool _) = TBool
literalType (LDuration _) = TRTime

-- | Get the type of a variable.
variableType :: Variable -> Validator VCLType
variableType (Variable parts) = case parts of
  ["req", _] -> return TString
  ["bereq", _] -> return TString
  ["beresp", _] -> return TString
  ["resp", _] -> return TString
  ["obj", _] -> return TString
  ["client", "ip"] -> return TIP
  ["server", "ip"] -> return TIP
  ["var", name] -> do
    ctx <- get
    case Map.lookup (Identifier name) (ctxLocalVars ctx) of
      Just t -> return t
      Nothing -> do
        addError $ UndefinedVariable (Variable parts)
        return TString
  _ -> return TString  -- Conservative default

-- | Determine result type of binary operation.
binOpResultType :: BinOp -> VCLType -> VCLType -> VCLType
binOpResultType op t1 t2 = case op of
  Add | t1 == TString || t2 == TString -> TString
      | t1 == TFloat || t2 == TFloat -> TFloat
      | otherwise -> TInteger
  Sub -> if t1 == TFloat || t2 == TFloat then TFloat else TInteger
  Mul -> if t1 == TFloat || t2 == TFloat then TFloat else TInteger
  Div -> if t1 == TFloat || t2 == TFloat then TFloat else TInteger
  Mod -> TInteger
  Eq -> TBool
  Ne -> TBool
  Lt -> TBool
  Le -> TBool
  Gt -> TBool
  Ge -> TBool
  And -> TBool
  Or -> TBool
  Match -> TBool
  NotMatch -> TBool
  Concat -> TString

-- | Determine result type of unary operation.
unOpResultType :: UnOp -> VCLType -> VCLType
unOpResultType Not _ = TBool
unOpResultType Neg t = t

-- ---------------------------------------------------------------------------
-- Variable Validation
-- ---------------------------------------------------------------------------

-- | Check if a variable is readable in the current context.
checkVariableReadable :: Variable -> Validator ()
checkVariableReadable var@(Variable parts) = do
  ctx <- get
  case (parts, ctxCurrentSub ctx) of
    (["req", _], Just RecvContext) -> return ()
    (["req", _], Just HashContext) -> return ()
    (["bereq", _], Just FetchContext) -> return ()
    (["bereq", _], Just ErrorContext) -> return ()
    (["beresp", _], Just FetchContext) -> return ()
    (["resp", _], Just DeliverContext) -> return ()
    (["resp", _], Just ErrorContext) -> return ()
    (["obj", _], Just DeliverContext) -> return ()
    (["obj", _], Just HitContext) -> return ()
    (["client", _], _) -> return ()
    (["server", _], _) -> return ()
    (["var", name], _) ->
      unless (Identifier name `Map.member` ctxLocalVars ctx) $
        addError $ UndefinedVariable var
    (_, Just subCtx) -> addError $ InvalidVariableContext var subCtx
    (_, Nothing) -> return ()  -- In custom subroutine, be lenient

-- | Check if a variable is writable in the current context.
checkVariableWritable :: Variable -> Validator ()
checkVariableWritable var@(Variable parts) = do
  ctx <- get
  case (parts, ctxCurrentSub ctx) of
    -- Read-only variables
    (["client", _], _) -> addError $ ReadOnlyVariable var
    (["server", _], _) -> addError $ ReadOnlyVariable var
    (["obj", "hits"], _) -> addError $ ReadOnlyVariable var
    (["obj", "lastuse"], _) -> addError $ ReadOnlyVariable var

    -- Context-specific writable variables
    (["req", _], Just RecvContext) -> return ()
    (["req", _], Just HashContext) -> return ()
    (["bereq", _], Just FetchContext) -> return ()
    (["bereq", _], Just ErrorContext) -> return ()
    (["beresp", _], Just FetchContext) -> return ()
    (["resp", _], Just DeliverContext) -> return ()
    (["resp", _], Just ErrorContext) -> return ()
    (["var", name], _) ->
      unless (Identifier name `Map.member` ctxLocalVars ctx) $
        addError $ UndefinedVariable var

    (_, Just subCtx) -> addError $ InvalidVariableContext var subCtx
    (_, Nothing) -> return ()

-- ---------------------------------------------------------------------------
-- Backend and ACL Validation
-- ---------------------------------------------------------------------------

-- | Validate a backend definition.
validateBackendM :: Backend -> Validator ()
validateBackendM (Backend _ props) = do
  -- Check for required properties
  let hasHost = any isHostProp props
  unless hasHost $
    addError $ InvalidOperation "Backend must have .host property"
  where
    isHostProp (BackendHost _) = True
    isHostProp _ = False

-- | Validate an ACL definition.
validateACLM :: ACL -> Validator ()
validateACLM (ACL _ entries) = do
  when (null entries) $
    addWarning $ UnusedVariable (Identifier "ACL has no entries")

-- | Validate a director definition.
validateDirectorM :: Director -> Validator ()
validateDirectorM (Director _ _ backends) = do
  ctx <- get
  -- Check that all referenced backends exist
  forM_ backends $ \backend ->
    unless (backend `Set.member` ctxBackends ctx) $
      addError $ UndefinedBackend backend

-- ---------------------------------------------------------------------------
-- Return Action Validation
-- ---------------------------------------------------------------------------

-- | Check if a return action is valid for the subroutine context.
isValidReturnAction :: Identifier -> SubroutineContext -> Bool
isValidReturnAction (Identifier action) subCtx = case subCtx of
  RecvContext -> action `elem` ["lookup", "pass", "pipe", "error", "synth", "hash"]
  HashContext -> action `elem` ["lookup", "hash"]
  HitContext -> action `elem` ["deliver", "pass", "restart", "synth", "error"]
  MissContext -> action `elem` ["fetch", "pass", "synth", "error"]
  PassContext -> action `elem` ["fetch", "synth", "error"]
  FetchContext -> action `elem` ["deliver", "deliver_stale", "restart", "error"]
  ErrorContext -> action `elem` ["deliver", "restart"]
  DeliverContext -> action `elem` ["deliver", "restart"]
  LogContext -> action `elem` ["deliver"]
  CustomContext -> True  -- Allow any action in custom subroutines

-- ---------------------------------------------------------------------------
-- Helper Functions
-- ---------------------------------------------------------------------------

-- | Add an error to the context.
addError :: ValidationError -> Validator ()
addError err = modify $ \c -> c { ctxErrors = err : ctxErrors c }

-- | Add a warning to the context.
addWarning :: ValidationWarning -> Validator ()
addWarning warn = modify $ \c -> c { ctxWarnings = warn : ctxWarnings c }

-- | Convert subroutine name to context.
subroutineNameToContext :: SubroutineName -> SubroutineContext
subroutineNameToContext VclRecv = RecvContext
subroutineNameToContext VclHash = HashContext
subroutineNameToContext VclHit = HitContext
subroutineNameToContext VclMiss = MissContext
subroutineNameToContext VclPass = PassContext
subroutineNameToContext VclFetch = FetchContext
subroutineNameToContext VclError = ErrorContext
subroutineNameToContext VclDeliver = DeliverContext
subroutineNameToContext VclLog = LogContext
subroutineNameToContext (CustomSub _) = CustomContext

-- | Check if subroutine is predefined.
isPredefinedSub :: SubroutineName -> Bool
isPredefinedSub (CustomSub _) = False
isPredefinedSub _ = True

-- | Show subroutine name as text.
showSubName :: SubroutineName -> Text
showSubName VclRecv = "vcl_recv"
showSubName VclHash = "vcl_hash"
showSubName VclHit = "vcl_hit"
showSubName VclMiss = "vcl_miss"
showSubName VclPass = "vcl_pass"
showSubName VclFetch = "vcl_fetch"
showSubName VclError = "vcl_error"
showSubName VclDeliver = "vcl_deliver"
showSubName VclLog = "vcl_log"
showSubName (CustomSub name) = name

-- | Show identifier as text.
showIdent :: Identifier -> Text
showIdent (Identifier name) = name
