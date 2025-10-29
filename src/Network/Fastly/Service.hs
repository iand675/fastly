{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly.Service
Description : Service management operations for the Fastly API
Copyright   : (c) 2018-2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module provides operations for managing Fastly services.

A service represents a single website or application configuration in Fastly.
Each service can have multiple versions, with typically one active version
serving production traffic.

= Service Workflow

1. Create a service with 'createService'
2. Add configuration (domains, backends, etc.) to a draft version
3. Validate the version with 'validateServiceVersion'
4. Activate the version with 'activateServiceVersion'
5. Monitor and update as needed

= Examples

@
-- List all services
services <- listServices client

-- Get a specific service with all details
service <- getServiceDetails client (ServiceId "service-id")

-- Search for a service by name
maybeService <- getServiceByName client "my-service"
@
-}

module Network.Fastly.Service
  ( -- * Listing Services
    listServices

    -- * Getting Services
  , getService
  , getServiceDetails
  , getServiceByName

    -- * Creating and Modifying Services
  , createService
  , updateService
  , deleteService

    -- * Service Versions
  , listServiceVersions
  , getServiceVersion
  , createServiceVersion
  , updateServiceVersion
  , cloneServiceVersion
  , validateServiceVersion
  , activateServiceVersion
  , deactivateServiceVersion
  , lockServiceVersion
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (setQueryString, urlEncodedBody)

import Network.Fastly.Client
import Network.Fastly.Types

-- ---------------------------------------------------------------------------
-- Service Operations
-- ---------------------------------------------------------------------------

-- | List all services for the authenticated account.
--
-- Returns a list of 'ServiceListItem' with basic information about each service.
-- For full details about a specific service, use 'getServiceDetails'.
--
-- ==== __Examples__
--
-- @
-- services <- listServices client
-- mapM_ (\\s -> putStrLn $ serviceListItemName s) services
-- @
listServices :: FastlyClient -> FastlyM [ServiceListItem]
listServices c = get c $ \r ->
  r { path = "/service" }

-- | Get basic information about a service.
--
-- This returns a 'Service' with some details but may not include all configuration.
-- For complete details including all versions and configurations, use 'getServiceDetails'.
getService :: FastlyClient
           -> ServiceId
           -> FastlyM Service
getService c (ServiceId sid) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid }

-- | Get complete details about a service.
--
-- This returns a 'Service' with full details including all versions,
-- the active version configuration, and other metadata.
--
-- ==== __Examples__
--
-- @
-- service <- getServiceDetails client (ServiceId "service-id")
-- case serviceActiveVersion service of
--   Nothing -> putStrLn "No active version"
--   Just v -> putStrLn $ "Active version: " ++ show (serviceVersionNumber v)
-- @
getServiceDetails :: FastlyClient
                  -> ServiceId
                  -> FastlyM Service
getServiceDetails c (ServiceId sid) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/details" }

-- | Search for a service by name.
--
-- Returns 'Nothing' if no service with the given name exists.
--
-- ==== __Examples__
--
-- @
-- maybeService <- getServiceByName client "my-production-service"
-- case maybeService of
--   Nothing -> putStrLn "Service not found"
--   Just s -> putStrLn $ "Found service: " ++ show (serviceId s)
-- @
getServiceByName :: FastlyClient
                 -> Text  -- ^ Service name to search for
                 -> FastlyM (Maybe Service)
getServiceByName c name = get c $ \r -> setQueryString [("name", Just $ encodeUtf8 name)] $
  r { path = "/service/search" }

-- | Create a new service.
--
-- The new service will have an initial version (version 1) created automatically.
-- You can then add configuration to this version before activating it.
--
-- ==== __Examples__
--
-- @
-- service <- createService client "My New Service" Nothing
-- putStrLn $ "Created service: " ++ show (serviceId service)
-- @
createService :: FastlyClient
              -> Text  -- ^ Service name
              -> Maybe Text  -- ^ Optional comment describing the service
              -> FastlyM Service
createService c name comment = post c $ \r -> urlEncodedBody params $
  r { path = "/service" }
  where
    params = ("name", encodeUtf8 name) : maybe [] (\c' -> [("comment", encodeUtf8 c')]) comment

-- | Update service properties.
--
-- You can update the service name and comment. This does not affect
-- service versions or their configurations.
updateService :: FastlyClient
              -> ServiceId
              -> Maybe Text  -- ^ New service name
              -> Maybe Text  -- ^ New comment
              -> FastlyM Service
updateService c (ServiceId sid) name comment = put c $ \r -> urlEncodedBody params $
  r { path = "/service/" <> encodeUtf8 sid }
  where
    params = maybe [] (\n -> [("name", encodeUtf8 n)]) name
          ++ maybe [] (\c' -> [("comment", encodeUtf8 c')]) comment

-- | Delete a service and all its versions.
--
-- __Warning:__ This operation cannot be undone. All versions and configurations
-- will be permanently deleted.
deleteService :: FastlyClient
              -> ServiceId
              -> FastlyM Service
deleteService c (ServiceId sid) = delete c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid }

-- ---------------------------------------------------------------------------
-- Service Version Operations
-- ---------------------------------------------------------------------------

-- | List all versions of a service.
--
-- Returns a list of 'ServiceBasicVersion' with information about each version.
listServiceVersions :: FastlyClient
                    -> ServiceId
                    -> FastlyM [ServiceBasicVersion]
listServiceVersions c (ServiceId sid) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version" }

-- | Get details about a specific service version.
--
-- Returns a 'ServiceVersion' with complete configuration including
-- backends, domains, VCLs, and all other settings.
getServiceVersion :: FastlyClient
                  -> ServiceId
                  -> ServiceVersionNumber
                  -> FastlyM ServiceVersion
getServiceVersion c (ServiceId sid) (ServiceVersionNumber v) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) }

-- | Create a new draft version based on the active version.
--
-- The new version will be a copy of the currently active version.
-- If there is no active version, it will be empty.
--
-- ==== __Examples__
--
-- @
-- version <- createServiceVersion client (ServiceId "service-id")
-- putStrLn $ "Created version: " ++ show (serviceVersionNumber version)
-- @
createServiceVersion :: FastlyClient
                     -> ServiceId
                     -> FastlyM ServiceVersion
createServiceVersion c (ServiceId sid) = post c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version" }

-- | Update a service version's comment.
--
-- The version must not be active or locked.
updateServiceVersion :: FastlyClient
                     -> ServiceId
                     -> ServiceVersionNumber
                     -> Text  -- ^ New comment
                     -> FastlyM ServiceVersion
updateServiceVersion c (ServiceId sid) (ServiceVersionNumber v) comment = put c $ \r -> urlEncodedBody [("comment", encodeUtf8 comment)] $
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) }

-- | Clone a service version.
--
-- Creates a new draft version that is a copy of the specified version.
-- This is useful for creating a new version based on a specific historical version
-- rather than the currently active one.
--
-- ==== __Examples__
--
-- @
-- -- Clone version 3 to create a new draft version
-- newVersion <- cloneServiceVersion client (ServiceId "service-id") (ServiceVersionNumber 3)
-- @
cloneServiceVersion :: FastlyClient
                    -> ServiceId
                    -> ServiceVersionNumber  -- ^ Version to clone
                    -> FastlyM ServiceVersion
cloneServiceVersion c (ServiceId sid) (ServiceVersionNumber v) = put c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/clone" }

-- | Validate a service version.
--
-- Checks that the version's configuration is valid and can be activated.
-- Returns validation errors if any are found.
--
-- It's recommended to validate a version before activating it to ensure
-- the configuration is correct.
--
-- ==== __Examples__
--
-- @
-- result <- validateServiceVersion client (ServiceId "service-id") (ServiceVersionNumber 2)
-- -- Check the result for validation errors
-- @
validateServiceVersion :: FastlyClient
                       -> ServiceId
                       -> ServiceVersionNumber
                       -> FastlyM ServiceVersion
validateServiceVersion c (ServiceId sid) (ServiceVersionNumber v) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/validate" }

-- | Activate a service version.
--
-- Makes the specified version the active version, which will start serving
-- production traffic. The previous active version will be deactivated.
--
-- __Note:__ The version must pass validation before it can be activated.
-- Use 'validateServiceVersion' to check for errors first.
--
-- ==== __Examples__
--
-- @
-- -- Validate first
-- _ <- validateServiceVersion client serviceId versionNum
-- -- Then activate
-- activeVersion <- activateServiceVersion client serviceId versionNum
-- @
activateServiceVersion :: FastlyClient
                       -> ServiceId
                       -> ServiceVersionNumber
                       -> FastlyM ServiceVersion
activateServiceVersion c (ServiceId sid) (ServiceVersionNumber v) = put c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/activate" }

-- | Deactivate the active service version.
--
-- This will stop the version from serving traffic. Typically you would
-- activate a different version rather than just deactivating.
deactivateServiceVersion :: FastlyClient
                         -> ServiceId
                         -> ServiceVersionNumber
                         -> FastlyM ServiceVersion
deactivateServiceVersion c (ServiceId sid) (ServiceVersionNumber v) = put c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/deactivate" }

-- | Lock a service version to prevent modifications.
--
-- Once locked, a version cannot be modified or deleted. This is useful
-- for preserving important historical configurations.
--
-- __Warning:__ This operation cannot be undone. A locked version cannot be unlocked.
lockServiceVersion :: FastlyClient
                   -> ServiceId
                   -> ServiceVersionNumber
                   -> FastlyM ServiceVersion
lockServiceVersion c (ServiceId sid) (ServiceVersionNumber v) = put c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/lock" }
