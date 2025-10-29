{-# LANGUAGE OverloadedStrings #-}

module ValidationSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Network.Fastly.VCL
import Network.Fastly.VCL.Types
import Network.Fastly.VCL.Validation

spec :: Spec
spec = do
  describe "VCL Validation" $ do
    variableValidationTests
    typeCheckingTests
    referenceValidationTests
    controlFlowTests
    returnActionTests

-- ---------------------------------------------------------------------------
-- Variable Validation Tests
-- ---------------------------------------------------------------------------

variableValidationTests :: Spec
variableValidationTests = describe "Variable validation" $ do
  it "accepts valid req variable in vcl_recv" $ do
    let stmt = Set (Variable ["req", "http", "Host"]) (stringLit "example.com")
    validateStatement RecvContext stmt `shouldSatisfy` isRight

  it "rejects req variable in vcl_deliver" $ do
    let stmt = Set (Variable ["req", "http", "Host"]) (stringLit "example.com")
    case validateStatement DeliverContext stmt of
      Left errs -> errs `shouldContain` [InvalidVariableContext (Variable ["req", "http", "Host"]) DeliverContext]
      Right _ -> expectationFailure "Should have failed validation"

  it "accepts resp variable in vcl_deliver" $ do
    let stmt = Set (Variable ["resp", "http", "X-Custom"]) (stringLit "value")
    validateStatement DeliverContext stmt `shouldSatisfy` isRight

  it "rejects resp variable in vcl_recv" $ do
    let stmt = Set (Variable ["resp", "http", "X-Custom"]) (stringLit "value")
    case validateStatement RecvContext stmt of
      Left errs -> length errs `shouldSatisfy` (> 0)
      Right _ -> expectationFailure "Should have failed validation"

  it "rejects write to read-only variable" $ do
    let stmt = Set (Variable ["client", "ip"]) (stringLit "127.0.0.1")
    case validateStatement RecvContext stmt of
      Left errs -> errs `shouldContain` [ReadOnlyVariable (Variable ["client", "ip"])]
      Right _ -> expectationFailure "Should have failed validation"

  it "accepts reading client.ip" $ do
    let stmt = Log (var ["client", "ip"])
    validateStatement RecvContext stmt `shouldSatisfy` isRight

  it "validates undefined local variable" $ do
    let stmt = Set (Variable ["var", "undefined"]) (stringLit "value")
    case validateStatement RecvContext stmt of
      Left errs -> errs `shouldContain` [UndefinedVariable (Variable ["var", "undefined"])]
      Right _ -> expectationFailure "Should have failed validation"

  it "accepts declared local variable" $ do
    let vclCode = T.unlines
          [ "sub vcl_recv {"
          , "  declare local var.foo STRING;"
          , "  set var.foo = \"test\";"
          , "}"
          ]
    case parseVCL vclCode of
      Left _ -> expectationFailure "Parse failed"
      Right vcl -> validateVCL vcl `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- Type Checking Tests
-- ---------------------------------------------------------------------------

typeCheckingTests :: Spec
typeCheckingTests = describe "Type checking" $ do
  it "accepts valid string comparison" $ do
    let expr = var ["req", "http", "Host"] .==. stringLit "example.com"
    validateExpr expr `shouldSatisfy` isRight

  it "accepts integer arithmetic" $ do
    let expr = intLit 10 .+. intLit 20
    validateExpr expr `shouldSatisfy` isRight

  it "accepts boolean logic" $ do
    let expr = boolLit True .&&. boolLit False
    validateExpr expr `shouldSatisfy` isRight

  it "rejects boolean NOT on integer" $ do
    let expr = notExpr (intLit 42)
    case validateExpr expr of
      Left errs -> length errs `shouldSatisfy` (> 0)
      Right _ -> expectationFailure "Should have failed validation"

  it "accepts if with boolean condition" $ do
    let stmt = ifStmt (boolLit True) [returnStmt]
    validateStatement RecvContext stmt `shouldSatisfy` isRight

  it "rejects if with non-boolean condition" $ do
    let stmt = ifStmt (intLit 42) [returnStmt]
    case validateStatement RecvContext stmt of
      Left errs -> any isTypeMismatch errs `shouldBe` True
      Right _ -> expectationFailure "Should have failed validation"

  it "validates declare with type mismatch" $ do
    let stmt = Declare (Identifier "foo") TInteger (Just $ stringLit "not an int")
    case validateStatement RecvContext stmt of
      Left errs -> any isTypeMismatch errs `shouldBe` True
      Right _ -> expectationFailure "Should have failed validation"

  it "accepts declare with matching type" $ do
    let stmt = Declare (Identifier "count") TInteger (Just $ intLit 0)
    validateStatement RecvContext stmt `shouldSatisfy` isRight

  it "validates synthetic requires string" $ do
    let stmt = Synthetic (intLit 404)
    case validateStatement RecvContext stmt of
      Left errs -> any isTypeMismatch errs `shouldBe` True
      Right _ -> expectationFailure "Should have failed validation"

-- ---------------------------------------------------------------------------
-- Reference Validation Tests
-- ---------------------------------------------------------------------------

referenceValidationTests :: Spec
referenceValidationTests = describe "Reference validation" $ do
  it "detects undefined subroutine call" $ do
    let stmt = Call (CustomSub "undefined_sub")
    case validateStatement RecvContext stmt of
      Left errs -> errs `shouldContain` [UndefinedSubroutine (CustomSub "undefined_sub")]
      Right _ -> expectationFailure "Should have failed validation"

  it "accepts call to defined subroutine" $ do
    let vclCode = T.unlines
          [ "sub my_custom_sub {"
          , "  log \"custom\";"
          , "}"
          , "sub vcl_recv {"
          , "  call my_custom_sub;"
          , "}"
          ]
    case parseVCL vclCode of
      Left _ -> expectationFailure "Parse failed"
      Right vcl -> validateVCL vcl `shouldSatisfy` isRight

  it "validates backend reference in director" $ do
    let vclCode = T.unlines
          [ "backend my_backend {"
          , "  .host = \"example.com\";"
          , "}"
          , "director my_director random {"
          , "  .backend = my_backend;"
          , "}"
          ]
    case parseVCL vclCode of
      Left _ -> expectationFailure "Parse failed"
      Right vcl -> validateVCL vcl `shouldSatisfy` isRight

  it "detects undefined backend in director" $ do
    let vclCode = T.unlines
          [ "director my_director random {"
          , "  .backend = undefined_backend;"
          , "}"
          ]
    case parseVCL vclCode of
      Left _ -> expectationFailure "Parse failed"
      Right vcl -> case validateVCL vcl of
        Left errs -> any isUndefinedBackend errs `shouldBe` True
        Right _ -> expectationFailure "Should have failed validation"

  it "detects duplicate backend definitions" $ do
    let vclCode = T.unlines
          [ "backend my_backend {"
          , "  .host = \"example1.com\";"
          , "}"
          , "backend my_backend {"
          , "  .host = \"example2.com\";"
          , "}"
          ]
    case parseVCL vclCode of
      Left _ -> expectationFailure "Parse failed"
      Right vcl -> case validateVCL vcl of
        Left errs -> any isDuplicateDefinition errs `shouldBe` True
        Right _ -> expectationFailure "Should have failed validation"

  it "detects duplicate subroutine definitions" $ do
    let vclCode = T.unlines
          [ "sub my_sub {"
          , "  log \"first\";"
          , "}"
          , "sub my_sub {"
          , "  log \"second\";"
          , "}"
          ]
    case parseVCL vclCode of
      Left _ -> expectationFailure "Parse failed"
      Right vcl -> case validateVCL vcl of
        Left errs -> any isDuplicateDefinition errs `shouldBe` True
        Right _ -> expectationFailure "Should have failed validation"

-- ---------------------------------------------------------------------------
-- Control Flow Tests
-- ---------------------------------------------------------------------------

controlFlowTests :: Spec
controlFlowTests = describe "Control flow validation" $ do
  it "detects unreachable code after return" $ do
    let vclCode = T.unlines
          [ "sub vcl_recv {"
          , "  return(pass);"
          , "  set req.http.Host = \"example.com\";"
          , "}"
          ]
    case parseVCL vclCode of
      Left _ -> expectationFailure "Parse failed"
      Right vcl -> case validateVCL vcl of
        Left errs -> errs `shouldContain` [UnreachableCode]
        Right _ -> expectationFailure "Should have failed validation"

  it "detects unreachable code after error" $ do
    let vclCode = T.unlines
          [ "sub vcl_recv {"
          , "  error 404;"
          , "  log \"unreachable\";"
          , "}"
          ]
    case parseVCL vclCode of
      Left _ -> expectationFailure "Parse failed"
      Right vcl -> case validateVCL vcl of
        Left errs -> errs `shouldContain` [UnreachableCode]
        Right _ -> expectationFailure "Should have failed validation"

  it "detects unreachable code after restart" $ do
    let vclCode = T.unlines
          [ "sub vcl_recv {"
          , "  restart;"
          , "  log \"unreachable\";"
          , "}"
          ]
    case parseVCL vclCode of
      Left _ -> expectationFailure "Parse failed"
      Right vcl -> case validateVCL vcl of
        Left errs -> errs `shouldContain` [UnreachableCode]
        Right _ -> expectationFailure "Should have failed validation"

  it "accepts reachable code after if without return" $ do
    let vclCode = T.unlines
          [ "sub vcl_recv {"
          , "  if (req.http.Host == \"example.com\") {"
          , "    set req.http.X-Custom = \"value\";"
          , "  }"
          , "  log \"reachable\";"
          , "}"
          ]
    case parseVCL vclCode of
      Left _ -> expectationFailure "Parse failed"
      Right vcl -> validateVCL vcl `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- Return Action Tests
-- ---------------------------------------------------------------------------

returnActionTests :: Spec
returnActionTests = describe "Return action validation" $ do
  it "accepts valid return(pass) in vcl_recv" $ do
    let stmt = returnWith "pass"
    validateStatement RecvContext stmt `shouldSatisfy` isRight

  it "accepts valid return(lookup) in vcl_recv" $ do
    let stmt = returnWith "lookup"
    validateStatement RecvContext stmt `shouldSatisfy` isRight

  it "rejects invalid return(deliver) in vcl_recv" $ do
    let stmt = returnWith "deliver"
    case validateStatement RecvContext stmt of
      Left errs -> any isInvalidReturnAction errs `shouldBe` True
      Right _ -> expectationFailure "Should have failed validation"

  it "accepts valid return(deliver) in vcl_deliver" $ do
    let stmt = returnWith "deliver"
    validateStatement DeliverContext stmt `shouldSatisfy` isRight

  it "accepts valid return(fetch) in vcl_miss" $ do
    let stmt = returnWith "fetch"
    validateStatement MissContext stmt `shouldSatisfy` isRight

  it "rejects invalid return(fetch) in vcl_recv" $ do
    let stmt = returnWith "fetch"
    case validateStatement RecvContext stmt of
      Left errs -> any isInvalidReturnAction errs `shouldBe` True
      Right _ -> expectationFailure "Should have failed validation"

-- ---------------------------------------------------------------------------
-- Helper Functions
-- ---------------------------------------------------------------------------

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isTypeMismatch :: ValidationError -> Bool
isTypeMismatch (TypeMismatch _ _ _) = True
isTypeMismatch _ = False

isUndefinedBackend :: ValidationError -> Bool
isUndefinedBackend (UndefinedBackend _) = True
isUndefinedBackend _ = False

isDuplicateDefinition :: ValidationError -> Bool
isDuplicateDefinition (DuplicateDefinition _) = True
isDuplicateDefinition _ = False

isInvalidReturnAction :: ValidationError -> Bool
isInvalidReturnAction (InvalidReturnAction _ _) = True
isInvalidReturnAction _ = False
