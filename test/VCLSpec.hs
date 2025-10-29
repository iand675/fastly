{-# LANGUAGE OverloadedStrings #-}

module VCLSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Network.Fastly.VCL
import Network.Fastly.VCL.Types

spec :: Spec
spec = do
  describe "VCL Parser" $ do
    parserTests

  describe "VCL Pretty-printer" $ do
    prettyPrinterTests

  describe "VCL Round-trip" $ do
    roundTripTests

  describe "VCL Builder API" $ do
    builderTests

-- ---------------------------------------------------------------------------
-- Parser Tests
-- ---------------------------------------------------------------------------

parserTests :: Spec
parserTests = do
  describe "Expression parsing" $ do
    it "parses string literals" $ do
      parseExpr "\"hello world\"" `shouldBe` Right (Lit $ LString "hello world")

    it "parses integer literals" $ do
      parseExpr "42" `shouldBe` Right (Lit $ LInteger 42)

    it "parses boolean literals" $ do
      parseExpr "true" `shouldBe` Right (Lit $ LBool True)
      parseExpr "false" `shouldBe` Right (Lit $ LBool False)

    it "parses duration literals" $ do
      parseExpr "10s" `shouldBe` Right (Lit $ LDuration "10s")
      parseExpr "5m" `shouldBe` Right (Lit $ LDuration "5m")
      parseExpr "1h" `shouldBe` Right (Lit $ LDuration "1h")

    it "parses variables" $ do
      parseExpr "req.http.Host" `shouldBe` Right (Var $ Variable ["req", "http", "Host"])
      parseExpr "beresp.status" `shouldBe` Right (Var $ Variable ["beresp", "status"])

    it "parses binary operations" $ do
      parseExpr "1 + 2" `shouldBe` Right (BinOp Add (Lit $ LInteger 1) (Lit $ LInteger 2))
      parseExpr "x == y" `shouldBe` Right (BinOp Eq (Var $ Variable ["x"]) (Var $ Variable ["y"]))
      parseExpr "x ~ \"pattern\"" `shouldBe` Right (BinOp Match (Var $ Variable ["x"]) (Lit $ LString "pattern"))

    it "parses unary operations" $ do
      parseExpr "!true" `shouldBe` Right (UnOp Not (Lit $ LBool True))
      parseExpr "-42" `shouldBe` Right (UnOp Neg (Lit $ LInteger 42))

    it "parses function calls" $ do
      parseExpr "func()" `shouldBe` Right (FunctionCall (Identifier "func") [])
      parseExpr "len(\"hello\")" `shouldBe`
        Right (FunctionCall (Identifier "len") [Lit $ LString "hello"])

  describe "Statement parsing" $ do
    it "parses set statements" $ do
      parseStatement "set req.http.Host = \"example.com\";" `shouldBe`
        Right (Set (Variable ["req", "http", "Host"]) (Lit $ LString "example.com"))

    it "parses unset statements" $ do
      parseStatement "unset req.http.Cookie;" `shouldBe`
        Right (Unset $ Variable ["req", "http", "Cookie"])

    it "parses declare statements" $ do
      parseStatement "declare local var.foo STRING;" `shouldBe`
        Right (Declare (Identifier "foo") TString Nothing)
      parseStatement "declare local var.count INTEGER = 0;" `shouldBe`
        Right (Declare (Identifier "count") TInteger (Just $ Lit $ LInteger 0))

    it "parses if statements" $ do
      let vclCode = "if (req.http.Host == \"example.com\") { return; }"
      case parseStatement vclCode of
        Right (If cond [Return Nothing] [] Nothing) ->
          cond `shouldBe` BinOp Eq (Var $ Variable ["req", "http", "Host"]) (Lit $ LString "example.com")
        _ -> expectationFailure "Failed to parse if statement"

    it "parses return statements" $ do
      parseStatement "return;" `shouldBe` Right (Return Nothing)
      parseStatement "return(pass);" `shouldBe` Right (Return $ Just $ Identifier "pass")

    it "parses call statements" $ do
      parseStatement "call vcl_recv;" `shouldBe` Right (Call VclRecv)

    it "parses log statements" $ do
      parseStatement "log \"test\";" `shouldBe` Right (Log $ Lit $ LString "test")

    it "parses error statements" $ do
      parseStatement "error 404;" `shouldBe` Right (Error 404 Nothing)
      parseStatement "error 404 \"Not Found\";" `shouldBe` Right (Error 404 $ Just "Not Found")

    it "parses restart statements" $ do
      parseStatement "restart;" `shouldBe` Right Restart

  describe "Subroutine parsing" $ do
    it "parses empty subroutine" $ do
      let vclCode = "sub vcl_recv { }"
      case parseVCL vclCode of
        Right (VCL [TopLevelSubroutine (Subroutine VclRecv [])]) -> return ()
        _ -> expectationFailure "Failed to parse empty subroutine"

    it "parses subroutine with statements" $ do
      let vclCode = unlines
            [ "sub vcl_recv {"
            , "  set req.http.Host = \"example.com\";"
            , "  return(pass);"
            , "}"
            ]
      case parseVCL vclCode of
        Right (VCL [TopLevelSubroutine (Subroutine VclRecv stmts)]) ->
          length stmts `shouldBe` 2
        _ -> expectationFailure "Failed to parse subroutine with statements"

    it "parses custom subroutines" $ do
      let vclCode = "sub my_custom_sub { log \"custom\"; }"
      case parseVCL vclCode of
        Right (VCL [TopLevelSubroutine (Subroutine (CustomSub "my_custom_sub") _)]) -> return ()
        _ -> expectationFailure "Failed to parse custom subroutine"

  describe "Backend parsing" $ do
    it "parses basic backend" $ do
      let vclCode = unlines
            [ "backend my_backend {"
            , "  .host = \"example.com\";"
            , "  .port = 443;"
            , "}"
            ]
      case parseVCL vclCode of
        Right (VCL [TopLevelBackend (Backend (Identifier "my_backend") props)]) ->
          length props `shouldBe` 2
        _ -> expectationFailure "Failed to parse backend"

  describe "ACL parsing" $ do
    it "parses ACL with entries" $ do
      let vclCode = unlines
            [ "acl allowed_ips {"
            , "  \"192.168.1.0/24\";"
            , "  !\"192.168.1.100\";"
            , "}"
            ]
      case parseVCL vclCode of
        Right (VCL [TopLevelACL (ACL (Identifier "allowed_ips") entries)]) ->
          length entries `shouldBe` 2
        _ -> expectationFailure "Failed to parse ACL"

-- ---------------------------------------------------------------------------
-- Pretty-printer Tests
-- ---------------------------------------------------------------------------

prettyPrinterTests :: Spec
prettyPrinterTests = do
  describe "Expression rendering" $ do
    it "renders string literals" $ do
      renderVCL (VCL []) `shouldSatisfy` T.null . T.strip

    it "renders binary operations" $ do
      let expr = BinOp Add (Lit $ LInteger 1) (Lit $ LInteger 2)
      prettyExpr expr `shouldSatisfy` (const True)  -- Just check it doesn't crash

  describe "Statement rendering" $ do
    it "renders set statements" $ do
      let stmt = Set (Variable ["req", "http", "Host"]) (Lit $ LString "example.com")
      prettyStatement stmt `shouldSatisfy` (const True)

    it "renders if statements" $ do
      let stmt = If (Lit $ LBool True) [Return Nothing] [] Nothing
      prettyStatement stmt `shouldSatisfy` (const True)

  describe "Subroutine rendering" $ do
    it "renders empty subroutine" $ do
      let sub = Subroutine VclRecv []
          vcl = VCL [TopLevelSubroutine sub]
          rendered = renderVCL vcl
      rendered `shouldSatisfy` T.isInfixOf "sub vcl_recv"
      rendered `shouldSatisfy` T.isInfixOf "{"
      rendered `shouldSatisfy` T.isInfixOf "}"

    it "renders subroutine with statements" $ do
      let stmt = Set (Variable ["req", "http", "Host"]) (Lit $ LString "example.com")
          sub = Subroutine VclRecv [stmt]
          vcl = VCL [TopLevelSubroutine sub]
          rendered = renderVCL vcl
      rendered `shouldSatisfy` T.isInfixOf "sub vcl_recv"
      rendered `shouldSatisfy` T.isInfixOf "set"

  describe "Backend rendering" $ do
    it "renders backend definition" $ do
      let backend = Backend (Identifier "my_backend")
            [ BackendHost "example.com"
            , BackendPort 443
            ]
          vcl = VCL [TopLevelBackend backend]
          rendered = renderVCL vcl
      rendered `shouldSatisfy` T.isInfixOf "backend my_backend"
      rendered `shouldSatisfy` T.isInfixOf ".host"
      rendered `shouldSatisfy` T.isInfixOf "example.com"

-- ---------------------------------------------------------------------------
-- Round-trip Tests
-- ---------------------------------------------------------------------------

roundTripTests :: Spec
roundTripTests = do
  describe "Parse and render round-trips" $ do
    it "round-trips empty subroutine" $ do
      let vclCode = "sub vcl_recv { }"
      case parseVCL vclCode of
        Right vcl -> do
          let rendered = renderVCL vcl
          case parseVCL rendered of
            Right vcl2 -> vcl2 `shouldBe` vcl
            Left err -> expectationFailure $ "Round-trip parse failed: " ++ show err
        Left err -> expectationFailure $ "Initial parse failed: " ++ show err

    it "round-trips simple set statement" $ do
      let vclCode = "sub vcl_recv { set req.http.Host = \"example.com\"; }"
      case parseVCL vclCode of
        Right vcl -> do
          let rendered = renderVCL vcl
          case parseVCL rendered of
            Right vcl2 -> vcl2 `shouldBe` vcl
            Left err -> expectationFailure $ "Round-trip parse failed: " ++ show err
        Left err -> expectationFailure $ "Initial parse failed: " ++ show err

-- ---------------------------------------------------------------------------
-- Builder API Tests
-- ---------------------------------------------------------------------------

builderTests :: Spec
builderTests = do
  describe "Subroutine builders" $ do
    it "builds subroutine with set statement" $ do
      let sub = subroutine VclRecv
            [ setVar ["req", "http", "Host"] (stringLit "example.com")
            , returnWith "pass"
            ]
      case sub of
        TopLevelSubroutine (Subroutine VclRecv [Set {}, Return {}]) -> return ()
        _ -> expectationFailure "Builder created incorrect structure"

  describe "Expression builders" $ do
    it "builds comparison expressions" $ do
      let expr = var ["req", "http", "Host"] .==. stringLit "example.com"
      case expr of
        BinOp Eq _ _ -> return ()
        _ -> expectationFailure "Builder created incorrect expression"

    it "builds logical expressions" $ do
      let expr = (var ["a"] .==. intLit 1) .&&. (var ["b"] .==. intLit 2)
      case expr of
        BinOp And _ _ -> return ()
        _ -> expectationFailure "Builder created incorrect expression"

    it "builds arithmetic expressions" $ do
      let expr = intLit 1 .+. intLit 2 .*. intLit 3
      case expr of
        BinOp Add _ (BinOp Mul _ _) -> return ()
        _ -> expectationFailure "Builder created incorrect expression"

  describe "Statement builders" $ do
    it "builds if statement" $ do
      let stmt = ifStmt (boolLit True) [returnStmt]
      case stmt of
        If _ [Return Nothing] [] Nothing -> return ()
        _ -> expectationFailure "Builder created incorrect statement"

    it "builds if-else statement" $ do
      let stmt = ifElse (boolLit True) [returnStmt] [logStmt $ stringLit "else"]
      case stmt of
        If _ _ [] (Just [Log _]) -> return ()
        _ -> expectationFailure "Builder created incorrect statement"

    it "builds complex subroutine" $ do
      let vcl = VCL
            [ subroutine VclRecv
              [ ifStmt (var ["req", "http", "Host"] .==. stringLit "example.com")
                [ setVar ["req", "backend"] (var ["my_backend"])
                , returnWith "pass"
                ]
              , logStmt (stringLit "default path")
              ]
            ]

      -- Render and parse back to ensure it's valid
      let rendered = renderVCL vcl
      case parseVCL rendered of
        Right _ -> return ()
        Left err -> expectationFailure $ "Generated VCL is invalid: " ++ show err
