{-# LANGUAGE OverloadedStrings #-}

{-|
This example demonstrates the VCL validation system and shows
common validation errors.
-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Network.Fastly.VCL

main :: IO ()
main = do
  putStrLn "==================================="
  putStrLn "VCL Validation Examples"
  putStrLn "==================================="

  example1ValidVCL
  example2InvalidVariableContext
  example3TypeMismatch
  example4ReadOnlyVariable
  example5UndefinedReference
  example6UnreachableCode
  example7InvalidReturnAction
  example8DuplicateDefinition
  example9ComplexValidation

  putStrLn "\n==================================="
  putStrLn "All validation examples completed!"
  putStrLn "==================================="

-- Example 1: Valid VCL passes validation
example1ValidVCL :: IO ()
example1ValidVCL = do
  putStrLn "\n=== Example 1: Valid VCL ==="

  let vclCode = unlines
        [ "backend origin {"
        , "  .host = \"example.com\";"
        , "  .port = 443;"
        , "}"
        , ""
        , "sub vcl_recv {"
        , "  if (req.http.Host == \"example.com\") {"
        , "    set req.http.X-Custom = \"value\";"
        , "    return(pass);"
        , "  }"
        , "  return(lookup);"
        , "}"
        ]

  putStrLn "VCL Code:"
  putStrLn vclCode

  case parseVCL vclCode of
    Left err -> do
      putStrLn "Parse error:"
      print err
    Right vcl -> case validateVCL vcl of
      Left errors -> do
        putStrLn "Validation errors:"
        mapM_ print errors
      Right _ -> putStrLn "✓ VCL is valid!"

-- Example 2: Invalid variable context
example2InvalidVariableContext :: IO ()
example2InvalidVariableContext = do
  putStrLn "\n=== Example 2: Invalid Variable Context ==="

  let vclCode = unlines
        [ "sub vcl_deliver {"
        , "  set req.http.Host = \"example.com\";"
        , "}"
        ]

  putStrLn "VCL Code:"
  putStrLn vclCode
  putStrLn "Error: req.* variables are not available in vcl_deliver"

  case parseVCL vclCode of
    Left err -> print err
    Right vcl -> case validateVCL vcl of
      Left errors -> do
        putStrLn "\nValidation errors:"
        mapM_ (\e -> putStrLn $ "  ✗ " ++ show e) errors
      Right _ -> putStrLn "✓ Valid"

-- Example 3: Type mismatch
example3TypeMismatch :: IO ()
example3TypeMismatch = do
  putStrLn "\n=== Example 3: Type Mismatch ==="

  let vclCode = unlines
        [ "sub vcl_recv {"
        , "  if (42) {"
        , "    return(pass);"
        , "  }"
        , "}"
        ]

  putStrLn "VCL Code:"
  putStrLn vclCode
  putStrLn "Error: if condition must be boolean, not integer"

  case parseVCL vclCode of
    Left err -> print err
    Right vcl -> case validateVCL vcl of
      Left errors -> do
        putStrLn "\nValidation errors:"
        mapM_ (\e -> putStrLn $ "  ✗ " ++ show e) errors
      Right _ -> putStrLn "✓ Valid"

-- Example 4: Read-only variable
example4ReadOnlyVariable :: IO ()
example4ReadOnlyVariable = do
  putStrLn "\n=== Example 4: Read-only Variable ==="

  let vclCode = unlines
        [ "sub vcl_recv {"
        , "  set client.ip = \"127.0.0.1\";"
        , "}"
        ]

  putStrLn "VCL Code:"
  putStrLn vclCode
  putStrLn "Error: client.ip is read-only"

  case parseVCL vclCode of
    Left err -> print err
    Right vcl -> case validateVCL vcl of
      Left errors -> do
        putStrLn "\nValidation errors:"
        mapM_ (\e -> putStrLn $ "  ✗ " ++ show e) errors
      Right _ -> putStrLn "✓ Valid"

-- Example 5: Undefined reference
example5UndefinedReference :: IO ()
example5UndefinedReference = do
  putStrLn "\n=== Example 5: Undefined Reference ==="

  let vclCode = unlines
        [ "sub vcl_recv {"
        , "  call my_custom_subroutine;"
        , "}"
        ]

  putStrLn "VCL Code:"
  putStrLn vclCode
  putStrLn "Error: my_custom_subroutine is not defined"

  case parseVCL vclCode of
    Left err -> print err
    Right vcl -> case validateVCL vcl of
      Left errors -> do
        putStrLn "\nValidation errors:"
        mapM_ (\e -> putStrLn $ "  ✗ " ++ show e) errors
      Right _ -> putStrLn "✓ Valid"

-- Example 6: Unreachable code
example6UnreachableCode :: IO ()
example6UnreachableCode = do
  putStrLn "\n=== Example 6: Unreachable Code ==="

  let vclCode = unlines
        [ "sub vcl_recv {"
        , "  return(pass);"
        , "  set req.http.Host = \"example.com\";"
        , "}"
        ]

  putStrLn "VCL Code:"
  putStrLn vclCode
  putStrLn "Error: Code after return is unreachable"

  case parseVCL vclCode of
    Left err -> print err
    Right vcl -> case validateVCL vcl of
      Left errors -> do
        putStrLn "\nValidation errors:"
        mapM_ (\e -> putStrLn $ "  ✗ " ++ show e) errors
      Right _ -> putStrLn "✓ Valid"

-- Example 7: Invalid return action
example7InvalidReturnAction :: IO ()
example7InvalidReturnAction = do
  putStrLn "\n=== Example 7: Invalid Return Action ==="

  let vclCode = unlines
        [ "sub vcl_recv {"
        , "  return(deliver);"
        , "}"
        ]

  putStrLn "VCL Code:"
  putStrLn vclCode
  putStrLn "Error: return(deliver) is not valid in vcl_recv"

  case parseVCL vclCode of
    Left err -> print err
    Right vcl -> case validateVCL vcl of
      Left errors -> do
        putStrLn "\nValidation errors:"
        mapM_ (\e -> putStrLn $ "  ✗ " ++ show e) errors
      Right _ -> putStrLn "✓ Valid"

-- Example 8: Duplicate definition
example8DuplicateDefinition :: IO ()
example8DuplicateDefinition = do
  putStrLn "\n=== Example 8: Duplicate Definition ==="

  let vclCode = unlines
        [ "backend my_backend {"
        , "  .host = \"example1.com\";"
        , "}"
        , ""
        , "backend my_backend {"
        , "  .host = \"example2.com\";"
        , "}"
        ]

  putStrLn "VCL Code:"
  putStrLn vclCode
  putStrLn "Error: backend my_backend defined twice"

  case parseVCL vclCode of
    Left err -> print err
    Right vcl -> case validateVCL vcl of
      Left errors -> do
        putStrLn "\nValidation errors:"
        mapM_ (\e -> putStrLn $ "  ✗ " ++ show e) errors
      Right _ -> putStrLn "✓ Valid"

-- Example 9: Complex validation with multiple errors
example9ComplexValidation :: IO ()
example9ComplexValidation = do
  putStrLn "\n=== Example 9: Complex Validation (Multiple Errors) ==="

  let vclCode = unlines
        [ "sub vcl_recv {"
        , "  set resp.http.X-Custom = \"value\";"  -- Error: resp.* not available
        , "  if (\"string\") {"                     -- Error: if needs boolean
        , "    call undefined_sub;"                 -- Error: subroutine not defined
        , "    return(pass);"
        , "    set req.http.Host = \"unreachable\";" -- Error: unreachable
        , "  }"
        , "  return(deliver);"                       -- Error: invalid return action
        , "}"
        ]

  putStrLn "VCL Code:"
  putStrLn vclCode
  putStrLn "\nThis VCL has multiple validation errors:"

  case parseVCL vclCode of
    Left err -> do
      putStrLn "Parse error:"
      print err
    Right vcl -> case validateVCL vcl of
      Left errors -> do
        putStrLn $ "\nFound " ++ show (length errors) ++ " validation errors:"
        mapM_ (\(i, e) -> putStrLn $ "  " ++ show i ++ ". " ++ show e) (zip [1..] errors)
      Right _ -> putStrLn "✓ Valid"

-- Bonus: Show how to validate during generation
exampleValidateWhileGenerating :: IO ()
exampleValidateWhileGenerating = do
  putStrLn "\n=== Bonus: Validate While Generating ==="

  -- Generate VCL programmatically
  let vcl = VCL
        [ subroutine VclRecv
          [ ifStmt (var ["req", "http", "Host"] .==. stringLit "example.com")
            [ setVar ["req", "backend"] (var ["origin"])
            , returnWith "pass"
            ]
          , returnWith "lookup"
          ]
        ]

  putStrLn "Generated VCL:"
  TIO.putStrLn $ renderVCL vcl

  case validateVCL vcl of
    Left errors -> do
      putStrLn "\nValidation errors:"
      mapM_ print errors
    Right _ -> putStrLn "\n✓ Generated VCL is valid!"
