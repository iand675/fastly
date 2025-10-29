{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly.Gzip
Description : Gzip compression configuration for the Fastly API
Copyright   : (c) 2018-2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module provides operations for managing gzip compression configurations.

Gzip configurations define which content should be compressed before being
sent to clients. Compression reduces bandwidth usage and improves page load
times for clients that support gzip encoding.

= Configuration

You can configure compression based on:

* Content types (e.g., @text/html@, @application/json@)
* File extensions (e.g., @html@, @css@, @js@)
* Cache conditions (VCL conditions)

= Examples

@
-- List all gzip configurations
configs <- listGzipConfigurations client serviceId versionNum

-- Get a specific configuration
config <- getGzipConfiguration client serviceId versionNum "my-gzip-config"

-- Delete a configuration
result <- deleteGzipConfiguration client serviceId versionNum "old-config"
@
-}

module Network.Fastly.Gzip
  ( -- * Gzip Configuration Operations
    listGzipConfigurations
  , getGzipConfiguration
  , createGzipConfiguration
  , updateGzipConfiguration
  , deleteGzipConfiguration
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (urlEncodedBody, urlEncode)

import Network.Fastly.Client
import Network.Fastly.Types

-- ---------------------------------------------------------------------------
-- Gzip Configuration Operations
-- ---------------------------------------------------------------------------

-- | List all gzip configurations for a service version.
--
-- ==== __Examples__
--
-- @
-- configs <- listGzipConfigurations client serviceId versionNum
-- mapM_ (\\cfg -> putStrLn $ gzipConfigurationName cfg) configs
-- @
listGzipConfigurations :: FastlyClient
                       -> ServiceId
                       -> ServiceVersionNumber
                       -> FastlyM [GzipConfiguration]
listGzipConfigurations c (ServiceId sid) (ServiceVersionNumber v) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/gzip" }

-- | Get a specific gzip configuration by name.
--
-- ==== __Examples__
--
-- @
-- config <- getGzipConfiguration client serviceId versionNum "default-gzip"
-- print $ gzipConfigurationContentTypes config
-- @
getGzipConfiguration :: FastlyClient
                     -> ServiceId
                     -> ServiceVersionNumber
                     -> Text  -- ^ Configuration name
                     -> FastlyM GzipConfiguration
getGzipConfiguration c (ServiceId sid) (ServiceVersionNumber v) name = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/gzip/" <> urlEncode False (encodeUtf8 name) }

-- | Create a new gzip configuration.
--
-- The service version must not be active or locked.
--
-- ==== __Examples__
--
-- @
-- let contentTypes = ContentTypes ["text/html", "application/json", "text/css"]
-- let extensions = Extensions ["html", "json", "css", "js"]
-- config <- createGzipConfiguration client serviceId versionNum "my-gzip" contentTypes extensions ""
-- @
createGzipConfiguration :: FastlyClient
                        -> ServiceId
                        -> ServiceVersionNumber
                        -> Text  -- ^ Configuration name
                        -> ContentTypes  -- ^ Content types to compress
                        -> Extensions  -- ^ File extensions to compress
                        -> Text  -- ^ Cache condition (optional, use empty string for none)
                        -> FastlyM GzipConfiguration
createGzipConfiguration c (ServiceId sid) (ServiceVersionNumber v) name contentTypes extensions cacheCondition = post c $ \r -> urlEncodedBody params $
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/gzip" }
  where
    ContentTypes cts = contentTypes
    Extensions exts = extensions
    params = [ ("name", encodeUtf8 name)
             , ("content_types", encodeUtf8 $ unwords cts)
             , ("extensions", encodeUtf8 $ unwords exts)
             ] ++ if null cacheCondition then [] else [("cache_condition", encodeUtf8 cacheCondition)]

-- | Update an existing gzip configuration.
--
-- The service version must not be active or locked.
updateGzipConfiguration :: FastlyClient
                        -> ServiceId
                        -> ServiceVersionNumber
                        -> Text  -- ^ Current configuration name
                        -> Maybe Text  -- ^ New name (Nothing to keep current)
                        -> Maybe ContentTypes  -- ^ New content types (Nothing to keep current)
                        -> Maybe Extensions  -- ^ New extensions (Nothing to keep current)
                        -> Maybe Text  -- ^ New cache condition (Nothing to keep current)
                        -> FastlyM GzipConfiguration
updateGzipConfiguration c (ServiceId sid) (ServiceVersionNumber v) name newName contentTypes extensions cacheCondition = put c $ \r -> urlEncodedBody params $
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/gzip/" <> urlEncode False (encodeUtf8 name) }
  where
    params = maybe [] (\n -> [("name", encodeUtf8 n)]) newName
          ++ maybe [] (\(ContentTypes cts) -> [("content_types", encodeUtf8 $ unwords cts)]) contentTypes
          ++ maybe [] (\(Extensions exts) -> [("extensions", encodeUtf8 $ unwords exts)]) extensions
          ++ maybe [] (\cc -> [("cache_condition", encodeUtf8 cc)]) cacheCondition

-- | Delete a gzip configuration.
--
-- The service version must not be active or locked.
--
-- ==== __Examples__
--
-- @
-- result <- deleteGzipConfiguration client serviceId versionNum "old-gzip-config"
-- putStrLn $ deleteGzipConfigurationResultStatus result
-- @
deleteGzipConfiguration :: FastlyClient
                        -> ServiceId
                        -> ServiceVersionNumber
                        -> Text  -- ^ Configuration name
                        -> FastlyM DeleteGzipConfigurationResult
deleteGzipConfiguration c (ServiceId sid) (ServiceVersionNumber v) name = delete c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/gzip/" <> urlEncode False (encodeUtf8 name) }
