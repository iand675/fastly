{-# LANGUAGE OverloadedStrings #-}

{-|
This example demonstrates how to use the VCL module to parse,
generate, and render Fastly VCL code.
-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Network.Fastly.VCL

-- Example 1: Parsing VCL from text
exampleParsing :: IO ()
exampleParsing = do
  putStrLn "\n=== Example 1: Parsing VCL ===\n"

  let vclCode = unlines
        [ "sub vcl_recv {"
        , "  if (req.http.Host == \"example.com\") {"
        , "    set req.backend = my_backend;"
        , "    return(pass);"
        , "  }"
        , "}"
        ]

  putStrLn "Input VCL:"
  putStrLn vclCode

  case parseVCL vclCode of
    Left err -> do
      putStrLn "Parse error:"
      print err
    Right vcl -> do
      putStrLn "Parsed successfully!"
      putStrLn "\nAST:"
      print vcl

-- Example 2: Generating VCL programmatically
exampleGeneration :: IO ()
exampleGeneration = do
  putStrLn "\n=== Example 2: Generating VCL ===\n"

  -- Build a VCL subroutine using the builder API
  let vcl = VCL
        [ subroutine VclRecv
          [ -- Check if the host is example.com
            ifStmt (var ["req", "http", "Host"] .==. stringLit "example.com")
              [ setVar ["req", "backend"] (var ["my_backend"])
              , returnWith "pass"
              ]
          , -- Log the request
            logStmt (stringLit "Received request for: " `concatExpr` var ["req", "http", "Host"])
          ]
        ]

  putStrLn "Generated VCL:"
  TIO.putStrLn $ renderVCL vcl
  where
    concatExpr e1 e2 = BinOp Concat e1 e2

-- Example 3: Round-trip parsing and rendering
exampleRoundTrip :: IO ()
exampleRoundTrip = do
  putStrLn "\n=== Example 3: Round-trip Parsing and Rendering ===\n"

  let originalVCL = unlines
        [ "sub vcl_deliver {"
        , "  set resp.http.X-Served-By = \"Fastly\";"
        , "  unset resp.http.X-Internal-Header;"
        , "}"
        ]

  putStrLn "Original VCL:"
  putStrLn originalVCL

  case parseVCL originalVCL of
    Left err -> do
      putStrLn "Parse error:"
      print err
    Right vcl -> do
      let rendered = renderVCL vcl
      putStrLn "Rendered VCL:"
      TIO.putStrLn rendered

      -- Parse again to verify round-trip
      case parseVCL rendered of
        Left err2 -> do
          putStrLn "Round-trip parse error:"
          print err2
        Right vcl2 -> do
          putStrLn "\nRound-trip successful!"
          putStrLn $ "ASTs match: " ++ show (vcl == vcl2)

-- Example 4: Complex VCL with multiple features
exampleComplex :: IO ()
exampleComplex = do
  putStrLn "\n=== Example 4: Complex VCL ===\n"

  let vcl = VCL
        [ -- Define a backend
          TopLevelBackend $ Backend (Identifier "my_backend")
            [ BackendHost "origin.example.com"
            , BackendPort 443
            , BackendSSL True
            , BackendConnectTimeout "1s"
            , BackendFirstByteTimeout "15s"
            ]

        , -- Define an ACL
          TopLevelACL $ ACL (Identifier "allowed_ips")
            [ ACLEntry False "192.168.1.0/24"
            , ACLEntry False "10.0.0.0/8"
            , ACLEntry True "10.0.1.100"  -- Negated entry
            ]

        , -- Define vcl_recv subroutine
          subroutine VclRecv
            [ -- Check ACL
              ifStmt (notExpr $ funcCall "client.ip" [] `inACL` var ["allowed_ips"])
                [ Error 403 (Just "Forbidden")
                ]

            , -- Set backend based on host
              ifStmt (var ["req", "http", "Host"] .==. stringLit "example.com")
                [ setVar ["req", "backend"] (var ["my_backend"])
                ]

            , -- Add custom header
              setVar ["req", "http", "X-Custom-Header"] (stringLit "custom-value")

            , -- Continue processing
              returnWith "lookup"
            ]

        , -- Define vcl_deliver subroutine
          subroutine VclDeliver
            [ -- Add cache status header
              setVar ["resp", "http", "X-Cache"]
                (ifExpr (var ["obj", "hits"] .>. intLit 0)
                  (stringLit "HIT")
                  (stringLit "MISS"))

            , -- Remove internal headers
              unsetVar ["resp", "http", "X-Internal-Debug"]

            , returnStmt
            ]
        ]

  putStrLn "Generated complex VCL:"
  TIO.putStrLn $ renderVCL vcl
  where
    inACL ip acl = FunctionCall (Identifier "in_acl") [ip, acl]
    ifExpr cond thenExpr elseExpr =
      FunctionCall (Identifier "if") [cond, thenExpr, elseExpr]

-- Example 5: Working with expressions
exampleExpressions :: IO ()
exampleExpressions = do
  putStrLn "\n=== Example 5: VCL Expressions ===\n"

  -- Arithmetic
  let expr1 = intLit 10 .+. intLit 20 .*. intLit 3
  putStrLn $ "Arithmetic: " ++ show expr1

  -- Comparisons
  let expr2 = var ["req", "http", "Host"] .==. stringLit "example.com"
  putStrLn $ "Comparison: " ++ show expr2

  -- Logical operations
  let expr3 = (var ["req", "http", "Host"] .==. stringLit "example.com")
          .&&. (var ["req", "url"] .~. stringLit "^/api/")
  putStrLn $ "Logical: " ++ show expr3

  -- Regex matching
  let expr4 = var ["req", "url"] .~. stringLit "\\.(jpg|png|gif)$"
  putStrLn $ "Regex: " ++ show expr4

  -- Function calls
  let expr5 = funcCall "std.tolower" [var ["req", "http", "Host"]]
  putStrLn $ "Function: " ++ show expr5

main :: IO ()
main = do
  putStrLn "==================================="
  putStrLn "Fastly VCL Module Examples"
  putStrLn "==================================="

  exampleParsing
  exampleGeneration
  exampleRoundTrip
  exampleComplex
  exampleExpressions

  putStrLn "\n==================================="
  putStrLn "All examples completed!"
  putStrLn "==================================="
