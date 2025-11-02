{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly
Description : Haskell client library for the Fastly CDN API
Copyright   : (c) 2018-2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

A comprehensive Haskell client library for the Fastly CDN API.

This library provides a type-safe interface to the Fastly API, allowing you to:

* Manage services and their configurations
* Purge cached content (by URL, surrogate key, or entire service)
* Configure edge dictionaries for dynamic content
* Manage domains and DNS configuration
* Configure compression and other performance settings

= Getting Started

To use this library, you'll need a Fastly API token. You can create one in
the Fastly web interface under Account > API Tokens.

== Simple Example

@
import Network.Fastly

main :: IO ()
main = do
  result <- fastly "your-api-token-here" $ \\client -> do
    -- List all services
    services <- listServices client
    return services

  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right services -> do
      putStrLn $ "Found " ++ show (length services) ++ " services"
      mapM_ print services
@

== Comprehensive Example

@
import Network.Fastly
import Control.Monad (when)

main :: IO ()
main = do
  let token = "your-api-token"
      serviceId = ServiceId "your-service-id"

  result <- fastly token $ \\client -> do
    -- Get service details
    service <- getServiceDetails client serviceId

    -- List dictionaries
    case serviceActiveVersion service of
      Nothing -> error "No active version"
      Just activeVer -> do
        let versionNum = serviceVersionNumber activeVer

        -- List edge dictionaries
        dicts <- listDictionaries client serviceId versionNum

        -- Purge a URL
        purgeResult <- purge client Instant "https://example.com/page.html"

        return (service, dicts, purgeResult)

  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (service, dicts, purgeResult) -> do
      putStrLn $ "Service: " ++ show (serviceName service)
      putStrLn $ "Dictionaries: " ++ show (length dicts)
      putStrLn $ "Purge ID: " ++ show (purgeResultId purgeResult)
@

= Module Organization

This library is organized into several modules:

* "Network.Fastly.Types" - Core types and data structures
* "Network.Fastly.Client" - HTTP client and authentication
* "Network.Fastly.Service" - Service management
* "Network.Fastly.Purge" - Cache purging operations
* "Network.Fastly.Dictionary" - Edge dictionary management
* "Network.Fastly.Domain" - Domain configuration
* "Network.Fastly.Gzip" - Compression configuration

You can import specific modules for more focused functionality, or just
import this module which re-exports everything.

= API Coverage

This library provides comprehensive coverage of the Fastly API including:

* ✓ Services and versions
* ✓ Purging (URL, surrogate key, purge all)
* ✓ Edge dictionaries (with batch operations)
* ✓ Domains (with DNS verification)
* ✓ Gzip configuration
* ✓ Cache status checking
* ✓ Public IP list

Future versions will add support for:

* Backends and healthchecks
* VCL management
* ACLs (Access Control Lists)
* TLS certificates
* Logging endpoints
* Historical stats
* And more...

= Error Handling

All API operations return results in the 'FastlyM' monad, which is an
'ExceptT' 'FastlyError' 'IO' monad. Use the 'fastly' helper function
to execute operations and get an @Either FastlyError a@ result.

Possible errors include:

* 'InvalidOrMissingRateLimitData' - API response missing rate limit headers
* 'JsonError' - Failed to decode API response
* 'InvalidUrl' - Malformed URL provided

= Rate Limiting

The Fastly API includes rate limiting. All responses include rate limit
information in the 'FastlyResponse' wrapper:

* 'fastlyResponseRateLimitRemaining' - Requests remaining in current window
* 'fastlyResponseRateLimitReset' - UTC epoch seconds when window resets

Currently, the high-level API functions don't expose this information,
but future versions may add support for rate limit-aware operations.

= Contributing

This library is open source and contributions are welcome! Visit the
project on GitHub at <https://github.com/iand675/fastly>.

-}

module Network.Fastly
  ( -- * Authentication and Client Creation
    -- ** Types
    FastlyAuth(..)
  , Username(..)
  , Password(..)
  , FastlyClient
  , FastlyResponse(..)
  , FastlyM
  , FastlyError(..)

    -- ** Client Functions
  , fastlyClient
  , fastly

    -- * Service Management
    -- ** Service Operations
  , listServices
  , getService
  , getServiceDetails
  , getServiceByName
  , createService
  , updateService
  , deleteService

    -- ** Service Version Operations
  , listServiceVersions
  , getServiceVersion
  , createServiceVersion
  , updateServiceVersion
  , cloneServiceVersion
  , validateServiceVersion
  , activateServiceVersion
  , deactivateServiceVersion
  , lockServiceVersion

    -- ** Service Types
  , ServiceId(..)
  , CustomerId(..)
  , ServiceVersionNumber(..)
  , Service(..)
  , ServiceListItem(..)
  , ServiceVersion(..)
  , ServiceBasicVersion(..)

    -- * Cache Purging
    -- ** Purge Operations
  , purge
  , purgeKey
  , purgeAll

    -- ** Purge Types
  , PurgeMode(..)
  , SurrogateKey(..)
  , PurgeResult(..)
  , PurgeAllResult(..)

    -- ** Utility Operations
  , edgeCheck
  , publicIpList

    -- ** Cache Types
  , CacheStatus(..)
  , CacheStatusRequest(..)
  , CacheStatusResponse(..)
  , Addresses(..)

    -- * Edge Dictionaries
    -- ** Dictionary Operations
  , listDictionaries
  , getDictionary
  , createDictionary
  , updateDictionary
  , deleteDictionary

    -- ** Dictionary Item Operations
  , listDictionaryItems
  , getDictionaryItem
  , createDictionaryItem
  , updateDictionaryItem
  , upsertDictionaryItem
  , deleteDictionaryItem
  , batchEditDictionaryItems

    -- ** Dictionary Types
  , DictionaryId(..)
  , Dictionary(..)
  , DictionaryItem(..)
  , DictionaryItemOp(..)
  , BatchEditResult(..)
  , DeleteDictionaryResult(..)
  , DeleteDictionaryItemResult(..)

    -- * Domain Management
    -- ** Domain Operations
  , listDomains
  , getDomain
  , createDomain
  , updateDomain
  , deleteDomain

    -- ** DNS Verification
  , checkDomainRecord
  , checkDomainRecords

    -- ** Domain Types
  , Domain(..)

    -- * Gzip Compression
    -- ** Gzip Operations
  , listGzipConfigurations
  , getGzipConfiguration
  , createGzipConfiguration
  , updateGzipConfiguration
  , deleteGzipConfiguration

    -- ** Gzip Types
  , GzipConfiguration(..)
  , ContentTypes(..)
  , Extensions(..)
  , DeleteGzipConfigurationResult(..)

    -- * Utility Types
  , Timestamp(..)
  , Boolean(..)
  ) where

-- Re-export all the modules
import Network.Fastly.Types
import Network.Fastly.Client
import Network.Fastly.Service
import Network.Fastly.Purge
import Network.Fastly.Dictionary
import Network.Fastly.Domain
import Network.Fastly.Gzip
