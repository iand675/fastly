{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly.Purge
Description : Cache purging operations for the Fastly API
Copyright   : (c) 2018-2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module provides operations for purging cached content from Fastly's edge servers.

Fastly supports several purging mechanisms:

* __URL Purging__: Purge a specific URL
* __Surrogate Key Purging__: Purge all URLs tagged with a specific key
* __Purge All__: Purge all cached content for a service

= Purge Modes

* 'Instant' purge (hard purge): Immediately removes content from cache
* 'Soft' purge: Marks content as stale but allows serving stale content if origin is unavailable

= Examples

@
-- Purge a specific URL
result <- purge client Instant "https://example.com/path"

-- Soft purge by surrogate key (recommended for large-scale purges)
result <- purgeKey client Soft serviceId (SurrogateKey "product-123")

-- Purge all content for a service (use with caution)
result <- purgeAll client serviceId
@
-}

module Network.Fastly.Purge
  ( -- * URL Purging
    purge

    -- * Surrogate Key Purging
  , purgeKey

    -- * Purge All
  , purgeAll

    -- * Utility Functions
  , publicIpList
  , edgeCheck
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client

import Network.Fastly.Client (MonadFastly(..), FastlyClient(..), getGlobalManager, pedanticDecode, FastlyM(..))
import Network.Fastly.Types

-- ---------------------------------------------------------------------------
-- Purging Operations
-- ---------------------------------------------------------------------------

-- | Purge a specific URL from the cache.
--
-- This removes the cached content for the specified URL from all Fastly edge servers.
-- Use 'Instant' mode for immediate removal, or 'Soft' mode to mark as stale while
-- still allowing stale content to be served if the origin is unavailable.
--
-- __Note:__ URL purging requires that the content was cached in the first place.
-- If the URL was never cached or has already expired, this will have no effect.
--
-- ==== __Examples__
--
-- @
-- -- Hard purge (immediate removal)
-- result <- purge client Instant "https://www.example.com/image.jpg"
-- putStrLn $ "Purge ID: " ++ purgeResultId result
--
-- -- Soft purge (mark as stale)
-- result <- purge client Soft "https://www.example.com/api/data"
-- @
purge :: PurgeMode  -- ^ Purge mode ('Instant' or 'Soft')
      -> String     -- ^ Full URL to purge
      -> FastlyM PurgeResult
purge mode url = FastlyM $ do
  result <- liftIO $ do
    m <- getGlobalManager
    case parseRequest url of
      Left _err -> return $ Left $ InvalidUrl url
      Right req -> do
        let f r = case mode of
              Instant -> r
              Soft -> r { requestHeaders = ("Fastly-Soft-Purge", "1") : requestHeaders r }
        r <- httpLbs (f req) { method = "PURGE" } m
        pedanticDecode r
  either throwError return result

-- | Purge all URLs tagged with a specific surrogate key.
--
-- Surrogate keys (also called cache tags) allow you to tag related content
-- and purge it all at once. This is more efficient than purging individual URLs
-- and is the recommended approach for most use cases.
--
-- For example, you might tag all content related to a product with @product-123@,
-- then purge all that content at once when the product is updated.
--
-- Soft purge is recommended for surrogate keys as it allows serving stale content
-- if your origin is experiencing issues.
--
-- ==== __Examples__
--
-- @
-- -- Soft purge all content tagged with "user-profile-42"
-- result <- purgeKey client Soft serviceId (SurrogateKey "user-profile-42")
--
-- -- Hard purge all content tagged with "breaking-news"
-- result <- purgeKey client Instant serviceId (SurrogateKey "breaking-news")
-- @
purgeKey :: PurgeMode       -- ^ Purge mode ('Instant' or 'Soft')
         -> ServiceId       -- ^ Service ID
         -> SurrogateKey    -- ^ Surrogate key to purge
         -> FastlyM PurgeResult
purgeKey mode (ServiceId sid) (SurrogateKey skey) = FastlyM $ do
  client <- ask
  result <- liftIO $ do
    m <- getGlobalManager
    let baseReq = fastlyClientBaseRequest client
        req = baseReq { method = "POST"
                      , path = "/service/" <> encodeUtf8 sid <> "/purge/" <> encodeUtf8 skey
                      , requestHeaders = case mode of
                          Instant -> requestHeaders baseReq
                          Soft -> ("Fastly-Soft-Purge", "1") : requestHeaders baseReq
                      }
    r <- httpLbs req m
    pedanticDecode r
  either throwError return result

-- | Purge all cached content for a service.
--
-- __Warning:__ This purges ALL cached content for the entire service,
-- which can cause a significant increase in origin traffic. Use this operation
-- with caution, preferably during off-peak hours.
--
-- This is useful when you need to:
--
-- * Clear all cache after a major deployment
-- * Remove all cached content after a security incident
-- * Reset cache during troubleshooting
--
-- Consider using surrogate keys instead for more targeted purges.
--
-- ==== __Examples__
--
-- @
-- result <- purgeAll client (ServiceId "service-id")
-- putStrLn $ "Purge status: " ++ purgeAllResultStatus result
-- @
purgeAll :: ServiceId  -- ^ Service ID
         -> FastlyM PurgeAllResult
purgeAll (ServiceId sid) = FastlyM $ do
  client <- ask
  result <- liftIO $ do
    m <- getGlobalManager
    let req = (fastlyClientBaseRequest client) { method = "POST"
                                                , path = "/service/" <> encodeUtf8 sid <> "/purge_all"
                                                }
    r <- httpLbs req m
    pedanticDecode r
  either throwError return result

-- ---------------------------------------------------------------------------
-- Utility Operations
-- ---------------------------------------------------------------------------

-- | Check the cache status of a URL at Fastly's edge.
--
-- This is useful for debugging caching issues. It returns information about
-- how Fastly would handle requests for the specified URL, including:
--
-- * Cache status (HIT, MISS, PASS, etc.)
-- * Response headers
-- * Serving POP location
-- * Response time
--
-- ==== __Examples__
--
-- @
-- statuses <- edgeCheck client "https://www.example.com/page.html"
-- mapM_ (\\s -> do
--   putStrLn $ "Server: " ++ cacheStatusServer s
--   putStrLn $ "Response time: " ++ show (cacheStatusResponseTime s)
-- ) statuses
-- @
edgeCheck :: Text  -- ^ URL to check
          -> FastlyM [CacheStatus]
edgeCheck url = fastlyGet $ \r -> setQueryString [("url", Just $ encodeUtf8 url)] $
  r { path = "/content/edge_check" }

-- | Get a list of Fastly's public IP address ranges.
--
-- This returns the IP ranges that Fastly uses for outbound connections
-- from its edge servers to origin servers. You can use this to:
--
-- * Configure firewall rules to allow traffic from Fastly
-- * Verify that requests are coming from Fastly
-- * Set up IP allowlists
--
-- ==== __Examples__
--
-- @
-- addrs <- publicIpList
-- putStrLn $ "Fastly uses " ++ show (length $ addresses addrs) ++ " IP ranges"
-- @
publicIpList :: FastlyM Addresses
publicIpList = FastlyM $ do
  result <- liftIO $ do
    m <- getGlobalManager
    case parseRequest "https://api.fastly.com/public-ip-list" of
      Nothing -> return $ Left $ InvalidUrl "https://api.fastly.com/public-ip-list"
      Just req -> do
        r <- httpLbs req m
        pedanticDecode r
  either throwError return result
