{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly.Client
Description : HTTP client and authentication for the Fastly API
Copyright   : (c) 2018-2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module provides the HTTP client functionality and authentication
mechanisms for interacting with the Fastly API.

= Usage

@
import Network.Fastly.Client

main :: IO ()
main = do
  result <- fastly "your-api-token" $ \\client -> do
    -- Make API calls here
    listServices client

  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right services -> print services
@
-}

module Network.Fastly.Client
  ( -- * Client Creation
    fastlyClient
  , fastly

    -- * HTTP Operations
  , get
  , post
  , put
  , delete
  , patch

    -- * Response Handling
  , pedanticDecode
  , readFromResponse

    -- * Manager
  , getGlobalManager
  ) where

import Control.Monad.Except
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lex.Integral (readDecimal_)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
import System.IO.Unsafe (unsafePerformIO)

import Network.Fastly.Types

-- | The base URL for the Fastly API.
fastlyRootRequest :: Request
fastlyRootRequest = parseRequest_ "https://api.fastly.com/"

-- | Global HTTP manager for connection pooling.
--
-- This is created once and reused across all requests for efficiency.
-- Uses TLS support for HTTPS connections.
{-# NOINLINE globalManager #-}
globalManager :: Manager
globalManager = unsafePerformIO newTlsManager

-- | Get the global HTTP connection manager.
getGlobalManager :: IO Manager
getGlobalManager = return globalManager

-- | Create a Fastly API client with the given authentication.
--
-- Currently only token-based authentication is supported.
-- Login-based authentication will be added in a future release.
--
-- ==== __Examples__
--
-- @
-- client <- fastlyClient (AuthToken "your-api-token")
-- @
fastlyClient :: FastlyAuth -> IO FastlyClient
fastlyClient (AuthToken t) = do
  return $
    FastlyClient
      (fastlyRootRequest
       { requestHeaders =
           ("Fastly-Key", encodeUtf8 t) :
           ("Accept", "application/json") : requestHeaders fastlyRootRequest
       })
fastlyClient (Login (Username _u) (Password _p)) = do
  error "Login-based authentication not yet implemented. Please use AuthToken."

-- | Execute Fastly API operations with a client created from an API token.
--
-- This is a convenience function that creates a client, executes the given
-- action, and returns the result.
--
-- ==== __Examples__
--
-- @
-- result <- fastly "your-api-token" $ \\client -> do
--   services <- listServices client
--   return services
-- @
fastly :: Text -> (FastlyClient -> FastlyM a) -> IO (Either FastlyError a)
fastly t f = do
  c <- fastlyClient $ AuthToken t
  runExceptT (f c)

-- | Pedantic JSON decoder that provides detailed error information.
--
-- This function attempts to decode the response body as JSON.
-- If decoding fails, it returns a 'JsonError' with both the decode error
-- and the raw JSON value (if it was valid JSON but failed type conversion).
pedanticDecode :: (Monad m, FromJSON a) => Response ByteString -> m (Either FastlyError a)
pedanticDecode r = return $ case eitherDecode $ responseBody r of
  Right result -> Right result
  Left err -> case eitherDecode $ responseBody r of
    Left _jsonErr -> Left $ JsonError err Nothing
    Right json    -> Left $ JsonError err (Just json)

-- | Extract rate limit information from response headers and combine with parsed body.
--
-- The Fastly API includes rate limiting information in response headers:
--
-- * @Fastly-RateLimit-Remaining@: Number of requests remaining in the current window
-- * @Fastly-RateLimit-Reset@: UTC epoch seconds when the window resets
readFromResponse :: Response a -> (Response a -> Either FastlyError b) -> Either FastlyError (FastlyResponse b)
readFromResponse r f = do
  case details of
    Nothing -> Left InvalidOrMissingRateLimitData
    Just (rem, res) -> do
      x <- f r
      return $ FastlyResponse rem res x
  where
    hs = responseHeaders r
    details = do
      remBs <- lookup "Fastly-RateLimit-Remaining" hs
      resBs <- lookup "Fastly-RateLimit-Reset" hs
      return (readDecimal_ remBs, readDecimal_ resBs)

-- | Execute an HTTP GET request.
--
-- The request modifier function can be used to set the path, query parameters,
-- and additional headers.
--
-- ==== __Examples__
--
-- @
-- services <- get client $ \\r -> r { path = "/service" }
-- @
get :: FromJSON a => FastlyClient -> (Request -> Request) -> FastlyM a
get c f = ExceptT $ do
  m <- getGlobalManager
  r <- httpLbs ((f $ fastlyClientBaseRequest c) { method = "GET" }) m
  pedanticDecode r

-- | Execute an HTTP POST request.
--
-- Use this for creating new resources.
post :: FromJSON a => FastlyClient -> (Request -> Request) -> FastlyM a
post c f = ExceptT $ do
  m <- getGlobalManager
  r <- httpLbs ((f $ fastlyClientBaseRequest c) { method = "POST" }) m
  pedanticDecode r

-- | Execute an HTTP PUT request.
--
-- Use this for full updates of existing resources.
put :: FromJSON a => FastlyClient -> (Request -> Request) -> FastlyM a
put c f = ExceptT $ do
  m <- getGlobalManager
  r <- httpLbs ((f $ fastlyClientBaseRequest c) { method = "PUT" }) m
  pedanticDecode r

-- | Execute an HTTP DELETE request.
--
-- Use this for deleting resources.
delete :: FromJSON a => FastlyClient -> (Request -> Request) -> FastlyM a
delete c f = ExceptT $ do
  m <- getGlobalManager
  r <- httpLbs ((f $ fastlyClientBaseRequest c) { method = "DELETE" }) m
  pedanticDecode r

-- | Execute an HTTP PATCH request.
--
-- Use this for partial updates of existing resources.
patch :: FromJSON a => FastlyClient -> (Request -> Request) -> FastlyM a
patch c f = ExceptT $ do
  m <- getGlobalManager
  r <- httpLbs ((f $ fastlyClientBaseRequest c) { method = "PATCH" }) m
  pedanticDecode r
