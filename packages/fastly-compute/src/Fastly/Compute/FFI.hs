{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

{-|
Module      : Fastly.Compute.FFI
Description : Low-level FFI bindings to Fastly Compute hostcalls
Copyright   : (c) 2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com

This module provides low-level FFI bindings to the Fastly Compute@Edge
WebAssembly hostcalls. These functions are the direct interface to the
Fastly runtime and should typically not be used directly. Use the
higher-level API in "Fastly.Compute" instead.

The Fastly Compute platform uses WASI with custom hostcalls to provide
HTTP request/response handling and other edge computing capabilities.
-}

module Fastly.Compute.FFI
  ( -- * Types
    RequestHandle
  , ResponseHandle
  , BodyHandle
  , FastlyStatus

    -- * Request Operations
  , fastly_req_downstream_client_request
  , fastly_req_body_downstream_get

    -- * Response Operations
  , fastly_resp_new
  , fastly_resp_send_downstream
  , fastly_resp_status_set

    -- * Body Operations
  , fastly_http_body_new
  , fastly_http_body_write
  , fastly_http_body_close

    -- * Header Operations
  , fastly_http_resp_header_insert

    -- * Error Handling
  , checkStatus
  , FastlyException(..)
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Exception
import Data.Typeable

-- | Handle to an HTTP request
newtype RequestHandle = RequestHandle Word32
  deriving (Eq, Show, Storable)

-- | Handle to an HTTP response
newtype ResponseHandle = ResponseHandle Word32
  deriving (Eq, Show, Storable)

-- | Handle to an HTTP body
newtype BodyHandle = BodyHandle Word32
  deriving (Eq, Show, Storable)

-- | Status code returned by Fastly hostcalls (0 = success)
type FastlyStatus = Word32

-- | Exception thrown when a Fastly hostcall fails
data FastlyException = FastlyException
  { fastlyExceptionFunction :: String
  , fastlyExceptionStatus   :: FastlyStatus
  } deriving (Show, Typeable)

instance Exception FastlyException

-- | Check a Fastly status code and throw an exception if it indicates failure
checkStatus :: String -> FastlyStatus -> IO ()
checkStatus funcName status
  | status == 0 = return ()
  | otherwise   = throwIO $ FastlyException funcName status

-- * Request Hostcalls

-- | Get the downstream client request
-- This retrieves the incoming HTTP request from the client
foreign import capi unsafe "fastly_req.h fastly_req_downstream_client_request"
  fastly_req_downstream_client_request
    :: Ptr RequestHandle  -- ^ Output: handle to the request
    -> Ptr BodyHandle     -- ^ Output: handle to the request body
    -> IO FastlyStatus

-- | Get the body of the downstream request
foreign import capi unsafe "fastly_req.h fastly_req_body_downstream_get"
  fastly_req_body_downstream_get
    :: RequestHandle      -- ^ Handle to the request
    -> Ptr BodyHandle     -- ^ Output: handle to the body
    -> IO FastlyStatus

-- * Response Hostcalls

-- | Create a new HTTP response
foreign import capi unsafe "fastly_resp.h fastly_resp_new"
  fastly_resp_new
    :: Ptr ResponseHandle -- ^ Output: handle to the new response
    -> IO FastlyStatus

-- | Send a response downstream to the client
foreign import capi unsafe "fastly_resp.h fastly_resp_send_downstream"
  fastly_resp_send_downstream
    :: ResponseHandle     -- ^ Handle to the response
    -> BodyHandle         -- ^ Handle to the response body
    -> Word32             -- ^ Streaming flag (0 = not streaming)
    -> IO FastlyStatus

-- | Set the HTTP status code of a response
foreign import capi unsafe "fastly_resp.h fastly_resp_status_set"
  fastly_resp_status_set
    :: ResponseHandle     -- ^ Handle to the response
    -> Word16             -- ^ HTTP status code (e.g., 200, 404)
    -> IO FastlyStatus

-- * Body Hostcalls

-- | Create a new HTTP body
foreign import capi unsafe "fastly_body.h fastly_http_body_new"
  fastly_http_body_new
    :: Ptr BodyHandle     -- ^ Output: handle to the new body
    -> IO FastlyStatus

-- | Write data to an HTTP body
foreign import capi unsafe "fastly_body.h fastly_http_body_write"
  fastly_http_body_write
    :: BodyHandle         -- ^ Handle to the body
    -> Ptr Word8          -- ^ Pointer to the data to write
    -> CSize              -- ^ Length of the data
    -> Word32             -- ^ Write mode (0 = back, 1 = front)
    -> Ptr CSize          -- ^ Output: number of bytes written
    -> IO FastlyStatus

-- | Close an HTTP body (finish writing)
foreign import capi unsafe "fastly_body.h fastly_http_body_close"
  fastly_http_body_close
    :: BodyHandle         -- ^ Handle to the body to close
    -> IO FastlyStatus

-- * Header Hostcalls

-- | Insert a header into an HTTP response
foreign import capi unsafe "fastly_resp.h fastly_http_resp_header_insert"
  fastly_http_resp_header_insert
    :: ResponseHandle     -- ^ Handle to the response
    -> Ptr Word8          -- ^ Header name (as bytes)
    -> CSize              -- ^ Length of header name
    -> Ptr Word8          -- ^ Header value (as bytes)
    -> CSize              -- ^ Length of header value
    -> IO FastlyStatus
