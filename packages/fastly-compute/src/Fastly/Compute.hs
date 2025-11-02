{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Fastly.Compute
Description : High-level API for Fastly Compute@Edge applications
Copyright   : (c) 2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com

This module provides a high-level, type-safe API for writing Fastly
Compute@Edge applications in Haskell compiled to WebAssembly.

= Quick Start

@
import Fastly.Compute
import qualified Data.ByteString as BS

main :: IO ()
main = runCompute $ \\req -> do
  return $ Response
    { responseStatus = 200
    , responseHeaders = [(\"Content-Type\", \"text/plain\")]
    , responseBody = \"Hello, World!\"
    }
@

= Architecture

Fastly Compute applications are WebAssembly modules that handle HTTP
requests at the edge. The application:

1. Receives an HTTP request from the client
2. Processes the request (routing, transformation, backend calls, etc.)
3. Returns an HTTP response to send back to the client

This all happens at Fastly's edge locations, providing low-latency
responses and the ability to customize request/response handling.
-}

module Fastly.Compute
  ( -- * Main Application Interface
    runCompute
  , ComputeHandler

    -- * Request Type
  , Request(..)
  , getDownstreamRequest

    -- * Response Type
  , Response(..)
  , sendResponse

    -- * Re-exports for convenience
  , ByteString
  ) where

import qualified Fastly.Compute.FFI as FFI
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Exception

-- | An HTTP request from the client
data Request = Request
  { requestMethod  :: !ByteString
  , requestPath    :: !ByteString
  , requestHeaders :: ![(ByteString, ByteString)]
  , requestBody    :: !ByteString
  } deriving (Show, Eq)

-- | An HTTP response to send to the client
data Response = Response
  { responseStatus  :: !Int
  , responseHeaders :: ![(ByteString, ByteString)]
  , responseBody    :: !ByteString
  } deriving (Show, Eq)

-- | Type of a Compute application handler function
-- Takes a Request and returns a Response (or an error)
type ComputeHandler = Request -> IO Response

-- | Get the incoming HTTP request from the downstream client
--
-- This retrieves the request that triggered this Compute invocation.
-- For a simple application that just needs to return a static response,
-- you may not need to inspect the request details.
getDownstreamRequest :: IO Request
getDownstreamRequest = do
  -- Allocate handles for the request and body
  alloca $ \reqHandlePtr ->
    alloca $ \bodyHandlePtr -> do
      -- Get the downstream request
      status <- FFI.fastly_req_downstream_client_request reqHandlePtr bodyHandlePtr
      FFI.checkStatus "fastly_req_downstream_client_request" status

      -- For now, return a minimal request
      -- A full implementation would parse headers, method, URL, etc.
      return $ Request
        { requestMethod  = "GET"
        , requestPath    = "/"
        , requestHeaders = []
        , requestBody    = ""
        }

-- | Send an HTTP response downstream to the client
sendResponse :: Response -> IO ()
sendResponse Response{..} = do
  -- Create a new response
  alloca $ \respHandlePtr -> do
    status <- FFI.fastly_resp_new respHandlePtr
    FFI.checkStatus "fastly_resp_new" status
    respHandle <- peek respHandlePtr

    -- Set the status code
    status' <- FFI.fastly_resp_status_set respHandle (fromIntegral responseStatus)
    FFI.checkStatus "fastly_resp_status_set" status'

    -- Add headers
    mapM_ (addHeader respHandle) responseHeaders

    -- Create a body and write the response content
    alloca $ \bodyHandlePtr -> do
      status'' <- FFI.fastly_http_body_new bodyHandlePtr
      FFI.checkStatus "fastly_http_body_new" status''
      bodyHandle <- peek bodyHandlePtr

      -- Write the body content
      unless (BS.null responseBody) $ do
        BSU.unsafeUseAsCStringLen responseBody $ \(ptr, len) -> do
          alloca $ \writtenPtr -> do
            status''' <- FFI.fastly_http_body_write
              bodyHandle
              (castPtr ptr)
              (fromIntegral len)
              0  -- Write to back
              writtenPtr
            FFI.checkStatus "fastly_http_body_write" status'''

      -- Send the response downstream
      statusSend <- FFI.fastly_resp_send_downstream respHandle bodyHandle 0
      FFI.checkStatus "fastly_resp_send_downstream" statusSend

  where
    -- Helper to add a header to the response
    addHeader :: FFI.ResponseHandle -> (ByteString, ByteString) -> IO ()
    addHeader respHandle (name, value) = do
      BSU.unsafeUseAsCStringLen name $ \(namePtr, nameLen) ->
        BSU.unsafeUseAsCStringLen value $ \(valuePtr, valueLen) -> do
          status <- FFI.fastly_http_resp_header_insert
            respHandle
            (castPtr namePtr)
            (fromIntegral nameLen)
            (castPtr valuePtr)
            (fromIntegral valueLen)
          FFI.checkStatus "fastly_http_resp_header_insert" status

-- | Run a Compute application
--
-- This is the main entry point for a Compute application. Pass your
-- handler function and it will:
--
-- 1. Retrieve the incoming HTTP request
-- 2. Call your handler with the request
-- 3. Send the response back to the client
--
-- Example:
--
-- @
-- main :: IO ()
-- main = runCompute $ \\req -> do
--   return $ Response 200 [(\"Content-Type\", \"text/html\")] \"<h1>Hello!</h1>\"
-- @
runCompute :: ComputeHandler -> IO ()
runCompute handler = do
  -- Get the incoming request
  req <- getDownstreamRequest

  -- Process it with the handler
  resp <- handler req `catch` handleError

  -- Send the response
  sendResponse resp

  where
    -- Error handler: convert exceptions to 500 responses
    handleError :: SomeException -> IO Response
    handleError e = return $ Response
      { responseStatus  = 500
      , responseHeaders = [("Content-Type", "text/plain")]
      , responseBody    = "Internal Server Error: " <> BS.pack (show e)
      }
