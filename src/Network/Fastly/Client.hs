{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

The core design uses a 'MonadFastly' type class that abstracts HTTP operations,
making the library testable and allowing for alternative implementations.

= Usage

@
import Network.Fastly.Client

main :: IO ()
main = do
  result <- runFastly "your-api-token" $ do
    -- Make API calls here
    listServices

  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right services -> print services
@
-}

module Network.Fastly.Client
  ( -- * Type Class
    MonadFastly(..)

    -- * Client Creation
  , FastlyClient(..)
  , fastlyClient
  , runFastly
  , fastly

    -- * FastlyM Implementation
  , FastlyM(..)
  , runFastlyM

    -- * Response Handling
  , pedanticDecode

    -- * Manager
  , getGlobalManager
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import System.IO.Unsafe (unsafePerformIO)

import Network.Fastly.Types

-- | Type class for Fastly API operations.
--
-- This abstracts the HTTP operations needed to interact with the Fastly API,
-- allowing for different implementations (production, testing, mocking, etc.).
--
-- The client connection and HTTP dispatch are managed internally by the instance.
class (Monad m, MonadIO m, MonadError FastlyError m) => MonadFastly m where
  -- | Execute an HTTP GET request.
  --
  -- The request modifier function can set the path, query parameters, and headers.
  fastlyGet :: FromJSON a => (Request -> Request) -> m a

  -- | Execute an HTTP POST request.
  --
  -- Used for creating new resources.
  fastlyPost :: FromJSON a => (Request -> Request) -> m a

  -- | Execute an HTTP PUT request.
  --
  -- Used for full updates of existing resources.
  fastlyPut :: FromJSON a => (Request -> Request) -> m a

  -- | Execute an HTTP DELETE request.
  --
  -- Used for deleting resources.
  fastlyDelete :: FromJSON a => (Request -> Request) -> m a

  -- | Execute an HTTP PATCH request.
  --
  -- Used for partial updates of existing resources.
  fastlyPatch :: FromJSON a => (Request -> Request) -> m a

-- | The standard Fastly monad implementation.
--
-- This is a newtype wrapper around a monad transformer stack that provides:
--
-- * 'ReaderT' for accessing the 'FastlyClient' configuration
-- * 'ExceptT' for error handling
-- * 'IO' as the base monad
newtype FastlyM a = FastlyM
  { unFastlyM :: ReaderT FastlyClient (ExceptT FastlyError IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError FastlyError
    , MonadReader FastlyClient
    )

-- | Run a 'FastlyM' computation with a given client.
runFastlyM :: FastlyClient -> FastlyM a -> IO (Either FastlyError a)
runFastlyM client action = runExceptT $ runReaderT (unFastlyM action) client

-- | The Fastly API client.
--
-- Contains the base HTTP request with authentication headers pre-configured.
data FastlyClient = FastlyClient
  { fastlyClientBaseRequest :: Request
  -- ^ The base HTTP request with auth headers configured
  }

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

-- | Instance of 'MonadFastly' for 'FastlyM'.
--
-- This implementation manages the HTTP client connection and performs
-- actual HTTP requests to the Fastly API.
instance MonadFastly FastlyM where
  fastlyGet f = FastlyM $ do
    client <- ask
    let req = (f $ fastlyClientBaseRequest client) { method = "GET" }
    result <- liftIO $ do
      m <- getGlobalManager
      r <- httpLbs req m
      pedanticDecode r
    either throwError return result

  fastlyPost f = FastlyM $ do
    client <- ask
    let req = (f $ fastlyClientBaseRequest client) { method = "POST" }
    result <- liftIO $ do
      m <- getGlobalManager
      r <- httpLbs req m
      pedanticDecode r
    either throwError return result

  fastlyPut f = FastlyM $ do
    client <- ask
    let req = (f $ fastlyClientBaseRequest client) { method = "PUT" }
    result <- liftIO $ do
      m <- getGlobalManager
      r <- httpLbs req m
      pedanticDecode r
    either throwError return result

  fastlyDelete f = FastlyM $ do
    client <- ask
    let req = (f $ fastlyClientBaseRequest client) { method = "DELETE" }
    result <- liftIO $ do
      m <- getGlobalManager
      r <- httpLbs req m
      pedanticDecode r
    either throwError return result

  fastlyPatch f = FastlyM $ do
    client <- ask
    let req = (f $ fastlyClientBaseRequest client) { method = "PATCH" }
    result <- liftIO $ do
      m <- getGlobalManager
      r <- httpLbs req m
      pedanticDecode r
    either throwError return result

-- | Execute Fastly API operations with a client created from an API token.
--
-- This is a convenience function that creates a client, executes the given
-- action, and returns the result.
--
-- ==== __Examples__
--
-- @
-- result <- runFastly "your-api-token" $ do
--   services <- listServices
--   return services
-- @
runFastly :: Text -> FastlyM a -> IO (Either FastlyError a)
runFastly token action = do
  client <- fastlyClient $ AuthToken token
  runFastlyM client action

-- | Deprecated: Use 'runFastly' instead.
--
-- This function is kept for backward compatibility but may be removed
-- in a future version.
fastly :: Text -> (FastlyClient -> FastlyM a) -> IO (Either FastlyError a)
fastly token f = do
  client <- fastlyClient $ AuthToken token
  runFastlyM client (f client)
{-# DEPRECATED fastly "Use runFastly instead, which doesn't require passing the client" #-}
