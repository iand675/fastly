{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Hello World example for Fastly Compute@Edge
Copyright   : (c) 2025 Ian Duncan
License     : BSD3

A simple "Hello, World!" application for Fastly Compute@Edge.

This demonstrates the minimal code needed to create a working
Compute application in Haskell.
-}

module Main where

import Fastly.Compute
import qualified Data.ByteString.Char8 as BS

-- | Main entry point for the Compute application
--
-- This function is called by the WebAssembly runtime when a request
-- arrives at the edge.
main :: IO ()
main = runCompute handler

-- | Request handler
--
-- This function processes incoming HTTP requests and returns responses.
-- In this simple example, we return the same response for all requests.
handler :: Request -> IO Response
handler req = do
  -- You can inspect the request here:
  -- - req.requestMethod for the HTTP method
  -- - req.requestPath for the URL path
  -- - req.requestHeaders for headers
  -- - req.requestBody for the request body

  -- Return a simple HTML response
  return $ Response
    { responseStatus  = 200
    , responseHeaders =
        [ ("Content-Type", "text/html; charset=utf-8")
        , ("X-Custom-Header", "Powered by Haskell + WebAssembly")
        ]
    , responseBody    = htmlResponse
    }

-- | HTML response content
htmlResponse :: ByteString
htmlResponse = BS.unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<head>"
  , "  <title>Haskell on Fastly Compute</title>"
  , "  <style>"
  , "    body { font-family: system-ui, sans-serif; max-width: 800px; margin: 50px auto; padding: 20px; }"
  , "    h1 { color: #FF282D; }"
  , "    .badge { background: #5E52BF; color: white; padding: 4px 8px; border-radius: 4px; font-size: 0.8em; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <h1>🎉 Hello from Haskell!</h1>"
  , "  <p>"
  , "    This page is served by a <span class=\"badge\">Haskell</span> application running on"
  , "    <span class=\"badge\">Fastly Compute@Edge</span>, compiled to"
  , "    <span class=\"badge\">WebAssembly</span>."
  , "  </p>"
  , "  <h2>How it works</h2>"
  , "  <ul>"
  , "    <li>GHC compiles Haskell to WebAssembly (wasm32-wasi target)</li>"
  , "    <li>The Wasm module runs at Fastly's edge locations</li>"
  , "    <li>FFI bindings provide access to Fastly's hostcalls</li>"
  , "    <li>High-level API makes it easy to write handlers</li>"
  , "  </ul>"
  , "  <h2>Capabilities</h2>"
  , "  <p>With this setup, you can:</p>"
  , "  <ul>"
  , "    <li>Handle HTTP requests and responses</li>"
  , "    <li>Route based on URLs, headers, or other criteria</li>"
  , "    <li>Make backend requests to origin servers</li>"
  , "    <li>Transform request and response data</li>"
  , "    <li>Use Haskell's type system for safe edge computing</li>"
  , "  </ul>"
  , "  <hr>"
  , "  <p><small>Powered by GHC " ++ show (__GLASGOW_HASKELL__) ++ " • WebAssembly • Fastly Compute</small></p>"
  , "</body>"
  , "</html>"
  ]
