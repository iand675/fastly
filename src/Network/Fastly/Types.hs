{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly.Types
Description : Core types and data structures for the Fastly API
Copyright   : (c) 2018-2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module contains all the core types, newtypes, and data structures used
throughout the Fastly API client library.
-}

module Network.Fastly.Types
  ( -- * Authentication
    FastlyAuth(..)
  , Username(..)
  , Password(..)

    -- * Client Types
  , FastlyClient(..)
  , FastlyResponse(..)
  , FastlyM
  , FastlyError(..)

    -- * Service Types
  , ServiceId(..)
  , ServiceVersionNumber(..)
  , Service(..)
  , ServiceListItem(..)
  , ServiceVersion(..)
  , ServiceBasicVersion(..)

    -- * Configuration Types
  , CustomerId(..)
  , DictionaryId(..)
  , Dictionary(..)
  , DictionaryItem(..)
  , DictionaryItemOp(..)
  , BatchEditResult(..)
  , DeleteDictionaryResult(..)
  , DeleteDictionaryItemResult(..)

    -- * Domain Types
  , Domain(..)

    -- * Gzip Types
  , GzipConfiguration(..)
  , ContentTypes(..)
  , Extensions(..)
  , DeleteGzipConfigurationResult(..)

    -- * Purge Types
  , PurgeMode(..)
  , PurgeResult(..)
  , PurgeAllResult(..)
  , SurrogateKey(..)

    -- * Cache Types
  , CacheStatus(..)
  , CacheStatusRequest(..)
  , CacheStatusResponse(..)

    -- * Network Types
  , Addresses(..)

    -- * Utility Types
  , Timestamp(..)
  , Boolean(..)

    -- * JSON Options
  , jsonOpts
  ) where

import Control.Applicative
import Control.Exception (Exception)
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import Data.IP (IPRange)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import GHC.Generics
import Network.HTTP.Client (Request)

-- | JSON options for deriving ToJSON/FromJSON instances.
-- Takes a prefix length to drop from field names and converts to snake_case.
jsonOpts :: Int -> Options
jsonOpts x = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop x }

-- ---------------------------------------------------------------------------
-- Authentication Types
-- ---------------------------------------------------------------------------

-- | Authentication credentials for the Fastly API.
--
-- Currently, only token-based authentication is supported.
-- Login-based authentication will be added in a future release.
data FastlyAuth
  = AuthToken Text  -- ^ API token authentication (recommended)
  | Login Username Password  -- ^ Username/password authentication (not yet implemented)

-- | Username for login-based authentication
newtype Username = Username Text

-- | Password for login-based authentication
newtype Password = Password Text

-- ---------------------------------------------------------------------------
-- Client Types
-- ---------------------------------------------------------------------------

-- | The Fastly API client.
--
-- Contains the base HTTP request with authentication headers pre-configured.
-- Create an instance using 'fastlyClient' from "Network.Fastly.Client".
data FastlyClient = FastlyClient
  { fastlyClientBaseRequest :: Request
  -- ^ The base HTTP request with auth headers configured
  }

-- | Response wrapper that includes Fastly rate limit information.
--
-- All API responses are wrapped in this type to provide access to
-- rate limiting headers returned by the API.
data FastlyResponse a = FastlyResponse
  { fastlyResponseRateLimitRemaining :: Int
  -- ^ Number of API calls remaining in the current rate limit window
  , fastlyResponseRateLimitReset     :: Int
  -- ^ UTC epoch seconds when the rate limit window resets
  , fastlyResponseValue              :: a
  -- ^ The actual response value
  } deriving (Show)

-- | The Fastly monad for API operations.
--
-- This is an 'ExceptT' transformer that handles 'FastlyError's and runs in 'IO'.
-- Most API operations return values in this monad.
type FastlyM a = ExceptT FastlyError IO a

-- | Errors that can occur when making Fastly API calls.
data FastlyError
  = InvalidOrMissingRateLimitData
  -- ^ The API response was missing required rate limit headers
  | JsonError String (Maybe Value)
  -- ^ Failed to decode JSON response. Includes error message and the raw JSON value if available.
  | InvalidUrl String
  -- ^ The provided URL was invalid or malformed
  deriving (Show, Eq)

instance Exception FastlyError

-- ---------------------------------------------------------------------------
-- Service Types
-- ---------------------------------------------------------------------------

-- | Unique identifier for a Fastly service.
newtype ServiceId = ServiceId Text
  deriving (Show, Eq, ToJSON, FromJSON)

-- | Service version number.
--
-- Each service can have multiple versions (draft, active, etc.).
-- The Fastly API represents versions as integers but sometimes returns them as strings.
newtype ServiceVersionNumber = ServiceVersionNumber Int
  deriving (Show, Eq, ToJSON)

instance FromJSON ServiceVersionNumber where
  parseJSON (String str) = return $ ServiceVersionNumber $ read $ unpack str
  parseJSON num@(Number _) = ServiceVersionNumber <$> parseJSON num
  parseJSON wat = typeMismatch "ServiceVersionNumber" wat

-- | Unique identifier for a customer account.
newtype CustomerId = CustomerId Text
  deriving (Show, Eq, ToJSON, FromJSON)

-- | Basic service information returned in list operations.
data ServiceListItem = ServiceListItem
  { serviceListItemCustomerId :: CustomerId
  -- ^ The customer that owns this service
  , serviceListItemVersions   :: [ServiceBasicVersion]
  -- ^ List of all versions of this service
  , serviceListItemVersion    :: ServiceVersionNumber
  -- ^ The current/latest version number
  , serviceListItemName       :: Text
  -- ^ Service name
  , serviceListItemId         :: ServiceId
  -- ^ Unique service identifier
  , serviceListItemComment    :: Text
  -- ^ User-provided comment describing the service
  } deriving (Show, Generic)

instance ToJSON ServiceListItem where
  toJSON = genericToJSON (jsonOpts 15)

instance FromJSON ServiceListItem where
  parseJSON = genericParseJSON (jsonOpts 15)

-- | Basic version information for a service version.
data ServiceBasicVersion = ServiceBasicVersion
  { serviceBasicVersionDeployed         :: Value
  -- ^ Whether this version is deployed
  , serviceBasicVersionLocked           :: Maybe Boolean
  -- ^ Whether this version is locked from editing
  , serviceBasicVersionTesting          :: Maybe Boolean
  -- ^ Whether this version is in testing
  , serviceBasicVersionActive           :: Value
  -- ^ Whether this version is active
  , serviceBasicVersionUpdatedAt        :: Maybe Timestamp
  -- ^ Last update timestamp
  , serviceBasicVersionDeletedAt        :: Maybe Timestamp
  -- ^ Deletion timestamp if soft-deleted
  , serviceBasicVersionServiceId        :: ServiceId
  -- ^ The service this version belongs to
  , serviceBasicVersionInheritServiceId :: Maybe ServiceId
  -- ^ Service ID this version inherits from, if any
  , serviceBasicVersionCreatedAt        :: Maybe Timestamp
  -- ^ Creation timestamp
  , serviceBasicVersionNumber           :: ServiceVersionNumber
  -- ^ The version number
  , serviceBasicVersionComment          :: Text
  -- ^ User-provided comment for this version
  , serviceBasicVersionStaging          :: Value
  -- ^ Whether this version is staging
  } deriving (Show, Generic)

instance ToJSON ServiceBasicVersion where
  toJSON = genericToJSON (jsonOpts 19)

instance FromJSON ServiceBasicVersion where
  parseJSON = genericParseJSON (jsonOpts 19)

-- | Complete service version with all configuration details.
--
-- This includes all the configuration objects (backends, domains, VCLs, etc.)
-- that are part of this service version.
data ServiceVersion = ServiceVersion
  { serviceVersionBackends        :: [Object]
  -- ^ List of backend configurations
  , serviceVersionGzips           :: [Object]
  -- ^ List of gzip configurations
  , serviceVersionDeployed        :: Value
  -- ^ Whether this version is deployed
  , serviceVersionVcls            :: [Value]
  -- ^ List of VCL configurations
  , serviceVersionLocked          :: Maybe Boolean
  -- ^ Whether editing is locked
  , serviceVersionSettings        :: Object
  -- ^ Service settings
  , serviceVersionMatches         :: [Value]
  -- ^ List of matches
  , serviceVersionTesting         :: Value
  -- ^ Whether in testing
  , serviceVersionActive          :: Value
  -- ^ Whether active
  , serviceVersionHeaders         :: [Object]
  -- ^ List of header configurations
  , serviceVersionServiceId       :: ServiceId
  -- ^ The service ID
  , serviceVersionResponseObjects :: [Object]
  -- ^ List of response objects
  , serviceVersionConditions      :: [Object]
  -- ^ List of conditions
  , serviceVersionNumber          :: ServiceVersionNumber
  -- ^ The version number
  , serviceVersionDomains         :: [Object]
  -- ^ List of domains
  , serviceVersionRequestSettings :: [Object]
  -- ^ List of request settings
  , serviceVersionComment         :: Text
  -- ^ Version comment
  , serviceVersionDirectors       :: [Object]
  -- ^ List of directors
  , serviceVersionCacheSettings   :: [Object]
  -- ^ List of cache settings
  , serviceVersionHealthchecks    :: [Object]
  -- ^ List of healthchecks
  , serviceVersionStaging         :: Value
  -- ^ Whether staging
  , serviceVersionWordpress       :: [Object]
  -- ^ WordPress configurations
  } deriving (Show, Generic)

instance ToJSON ServiceVersion where
  toJSON = genericToJSON (jsonOpts 14)

instance FromJSON ServiceVersion where
  parseJSON = genericParseJSON (jsonOpts 14)

-- | Complete service information including active and all versions.
data Service = Service
  { serviceId            :: ServiceId
  -- ^ Unique service identifier
  , serviceName          :: Text
  -- ^ Service name
  , serviceCustomerId    :: CustomerId
  -- ^ Customer that owns this service
  , serviceComment       :: Text
  -- ^ Service description
  , serviceActiveVersion :: Maybe ServiceVersion
  -- ^ The currently active version, if any
  , serviceVersion       :: Maybe ServiceVersion
  -- ^ The current working version, if any
  , serviceVersions      :: [ServiceBasicVersion]
  -- ^ All versions of this service
  } deriving (Show, Generic)

instance ToJSON Service where
  toJSON = genericToJSON (jsonOpts 7)

instance FromJSON Service where
  parseJSON = genericParseJSON (jsonOpts 7)

-- ---------------------------------------------------------------------------
-- Dictionary Types
-- ---------------------------------------------------------------------------

-- | Unique identifier for an edge dictionary.
newtype DictionaryId = DictionaryId Text
  deriving (Show, Eq, ToJSON, FromJSON)

-- | An edge dictionary for storing key-value data.
--
-- Edge dictionaries allow you to store data at the edge that can be
-- accessed from VCL. They can be updated without deploying a new service version.
data Dictionary = Dictionary
  { dictionaryCreatedAt :: Timestamp
  -- ^ When the dictionary was created
  , dictionaryDeletedAt :: Maybe Timestamp
  -- ^ When the dictionary was deleted (soft delete)
  , dictionaryId        :: DictionaryId
  -- ^ Unique dictionary identifier
  , dictionaryName      :: Text
  -- ^ Dictionary name
  , dictionaryServiceId :: ServiceId
  -- ^ Service this dictionary belongs to
  , dictionaryUpdatedAt :: Maybe Timestamp
  -- ^ Last update time
  , dictionaryVersion   :: ServiceVersionNumber
  -- ^ Service version this dictionary is attached to
  } deriving (Show, Generic)

instance ToJSON Dictionary where
  toJSON = genericToJSON (jsonOpts 10)

instance FromJSON Dictionary where
  parseJSON = genericParseJSON (jsonOpts 10)

-- | A single key-value item in an edge dictionary.
data DictionaryItem = DictionaryItem
  { dictionaryItemDictionaryId :: DictionaryId
  -- ^ The dictionary this item belongs to
  , dictionaryItemServiceId    :: ServiceId
  -- ^ The service this item's dictionary belongs to
  , dictionaryItemItemKey      :: Text
  -- ^ The item's key
  , dictionaryItemItemValue    :: Text
  -- ^ The item's value
  , dictionaryItemCreatedAt    :: Timestamp
  -- ^ When this item was created
  , dictionaryItemUpdatedAt    :: Timestamp
  -- ^ When this item was last updated
  , dictionaryItemDeletedAt    :: Maybe Timestamp
  -- ^ When this item was deleted (soft delete)
  } deriving (Show, Generic)

instance ToJSON DictionaryItem where
  toJSON = genericToJSON (jsonOpts 14)

instance FromJSON DictionaryItem where
  parseJSON = genericParseJSON (jsonOpts 14)

-- | Operations for batch editing dictionary items.
data DictionaryItemOp
  = CreateItem { dictionaryOpKey :: Text, dictionaryOpValue :: Text }
  -- ^ Create a new item
  | UpdateItem { dictionaryOpKey :: Text, dictionaryOpValue :: Text }
  -- ^ Update an existing item (fails if it doesn't exist)
  | UpsertItem { dictionaryOpKey :: Text, dictionaryOpValue :: Text }
  -- ^ Create or update an item
  | DeleteItem { dictionaryOpKey :: Text }
  -- ^ Delete an item

instance ToJSON DictionaryItemOp where
  toJSON (CreateItem k v) = object [ "op" .= String "create"
                                   , "item_key" .= k
                                   , "item_value" .= v
                                   ]
  toJSON (UpdateItem k v) = object [ "op" .= String "update"
                                   , "item_key" .= k
                                   , "item_value" .= v
                                   ]
  toJSON (UpsertItem k v) = object [ "op" .= String "upsert"
                                   , "item_key" .= k
                                   , "item_value" .= v
                                   ]
  toJSON (DeleteItem k)   = object [ "op" .= String "delete"
                                   , "item_key" .= k
                                   ]

-- | Result of a batch edit operation.
data BatchEditResult = BatchEditResult
  { batchEditResultStatus :: Text
  -- ^ Status message (typically "ok")
  } deriving (Show, Generic)

instance ToJSON BatchEditResult where
  toJSON = genericToJSON (jsonOpts 15)

instance FromJSON BatchEditResult where
  parseJSON = genericParseJSON (jsonOpts 15)

-- | Result of deleting a dictionary.
data DeleteDictionaryResult = DeleteDictionaryResult
  { deleteDictionaryResultStatus :: Text
  -- ^ Status message
  } deriving (Show, Generic)

instance ToJSON DeleteDictionaryResult where
  toJSON = genericToJSON (jsonOpts 22)

instance FromJSON DeleteDictionaryResult where
  parseJSON = genericParseJSON (jsonOpts 22)

-- | Result of deleting a dictionary item.
data DeleteDictionaryItemResult = DeleteDictionaryItemResult
  { deleteDictionaryItemResultStatus :: Text
  -- ^ Status message
  } deriving (Show, Generic)

instance ToJSON DeleteDictionaryItemResult where
  toJSON = genericToJSON (jsonOpts 26)

instance FromJSON DeleteDictionaryItemResult where
  parseJSON = genericParseJSON (jsonOpts 26)

-- ---------------------------------------------------------------------------
-- Domain Types
-- ---------------------------------------------------------------------------

-- | A domain name associated with a service.
data Domain = Domain
  { domainComment   :: Text
  -- ^ User-provided comment
  , domainLocked    :: Bool
  -- ^ Whether this domain is locked from editing
  , domainName      :: Text
  -- ^ The domain name
  , domainServiceId :: ServiceId
  -- ^ Service this domain belongs to
  , domainVersion   :: ServiceVersionNumber
  -- ^ Service version this domain is attached to
  , domainUpdatedAt :: Maybe Timestamp
  -- ^ Last update time
  , domainDeletedAt :: Maybe Timestamp
  -- ^ Deletion time (soft delete)
  , domainCreatedAt :: Maybe Timestamp
  -- ^ Creation time
  } deriving (Show, Generic)

instance ToJSON Domain where
  toJSON = genericToJSON (jsonOpts 6)

instance FromJSON Domain where
  parseJSON = genericParseJSON (jsonOpts 6)

-- ---------------------------------------------------------------------------
-- Gzip Types
-- ---------------------------------------------------------------------------

-- | Space-delimited list of content types to compress.
newtype ContentTypes = ContentTypes [Text]
  deriving (Show)

instance ToJSON ContentTypes where
  toJSON (ContentTypes ts) = String $ mconcat $ map (<> " ") ts

instance FromJSON ContentTypes where
  parseJSON (String s) = return $ ContentTypes $ filter (not . null) $ words (unpack s)
  parseJSON wat = typeMismatch "ContentTypes" wat

-- | Space-delimited list of file extensions to compress.
newtype Extensions = Extensions [Text]
  deriving (Show)

instance ToJSON Extensions where
  toJSON (Extensions ts) = String $ mconcat $ map (<> " ") ts

instance FromJSON Extensions where
  parseJSON (String s) = return $ Extensions $ filter (not . null) $ words (unpack s)
  parseJSON wat = typeMismatch "Extensions" wat

-- | Gzip compression configuration.
data GzipConfiguration = GzipConfiguration
  { gzipConfigurationCacheCondition :: Text
  -- ^ Cache condition under which to apply gzip
  , gzipConfigurationContentTypes   :: ContentTypes
  -- ^ Content types to compress
  , gzipConfigurationExtensions     :: Extensions
  -- ^ File extensions to compress
  , gzipConfigurationName           :: Text
  -- ^ Configuration name
  , gzipConfigurationServiceId      :: ServiceId
  -- ^ Service this configuration belongs to
  , gzipConfigurationVersion        :: ServiceVersionNumber
  -- ^ Service version
  } deriving (Show, Generic)

instance ToJSON GzipConfiguration where
  toJSON = genericToJSON (jsonOpts 17)

instance FromJSON GzipConfiguration where
  parseJSON = genericParseJSON (jsonOpts 17)

-- | Result of deleting a gzip configuration.
data DeleteGzipConfigurationResult = DeleteGzipConfigurationResult
  { deleteGzipConfigurationResultStatus :: Text
  -- ^ Status message
  } deriving (Show, Generic)

instance ToJSON DeleteGzipConfigurationResult where
  toJSON = genericToJSON (jsonOpts 29)

instance FromJSON DeleteGzipConfigurationResult where
  parseJSON = genericParseJSON (jsonOpts 29)

-- ---------------------------------------------------------------------------
-- Purge Types
-- ---------------------------------------------------------------------------

-- | Cache purge mode.
data PurgeMode
  = Instant  -- ^ Hard purge - content is immediately removed
  | Soft     -- ^ Soft purge - content is marked as stale but can still be served
  deriving (Show, Eq)

-- | A surrogate key for cache tagging.
--
-- Surrogate keys allow you to tag cached content and purge by tag
-- rather than by URL.
newtype SurrogateKey = SurrogateKey Text
  deriving (Show, Eq)

-- | Result of a cache purge operation.
data PurgeResult = PurgeResult
  { purgeResultStatus :: Text
  -- ^ Status message (typically "ok")
  , purgeResultId     :: Text
  -- ^ Unique identifier for this purge operation
  } deriving (Show, Generic)

instance ToJSON PurgeResult where
  toJSON = genericToJSON (jsonOpts 11)

instance FromJSON PurgeResult where
  parseJSON = genericParseJSON (jsonOpts 11)

-- | Result of a purge-all operation.
data PurgeAllResult = PurgeAllResult
  { purgeAllResultStatus :: Text
  -- ^ Status message (typically "ok")
  } deriving (Show, Generic)

instance ToJSON PurgeAllResult where
  toJSON = genericToJSON (jsonOpts 14)

instance FromJSON PurgeAllResult where
  parseJSON = genericParseJSON (jsonOpts 14)

-- ---------------------------------------------------------------------------
-- Cache Status Types
-- ---------------------------------------------------------------------------

-- | Information about a URL's cache status.
data CacheStatus = CacheStatus
  { cacheStatusHash         :: Text
  -- ^ Cache key hash
  , cacheStatusResponse     :: CacheStatusResponse
  -- ^ Response information
  , cacheStatusResponseTime :: Double
  -- ^ Response time in seconds
  , cacheStatusServer       :: Text
  -- ^ Serving POP/server
  , cacheStatusRequest      :: CacheStatusRequest
  -- ^ Request information
  } deriving (Show, Generic)

instance ToJSON CacheStatus where
  toJSON = genericToJSON (jsonOpts 11)

instance FromJSON CacheStatus where
  parseJSON = genericParseJSON (jsonOpts 11)

-- | Request information for cache status check.
data CacheStatusRequest = CacheStatusRequest
  { cacheStatusRequestUrl     :: Maybe Text
  -- ^ The requested URL
  , cacheStatusRequestHeaders :: HashMap Text Text
  -- ^ Request headers
  , cacheStatusRequestMethod  :: Maybe Text
  -- ^ HTTP method
  } deriving (Show, Generic)

instance ToJSON CacheStatusRequest where
  toJSON = genericToJSON (jsonOpts 18)

instance FromJSON CacheStatusRequest where
  parseJSON = genericParseJSON (jsonOpts 18)

-- | Response information for cache status check.
data CacheStatusResponse = CacheStatusResponse
  { cacheStatusResponseStatus :: Int
  -- ^ HTTP status code
  , cacheStatusResponseHeaders :: HashMap Text Text
  -- ^ Response headers
  } deriving (Show, Generic)

instance ToJSON CacheStatusResponse where
  toJSON = genericToJSON (jsonOpts 19)

instance FromJSON CacheStatusResponse where
  parseJSON = genericParseJSON (jsonOpts 19)

-- ---------------------------------------------------------------------------
-- Network Types
-- ---------------------------------------------------------------------------

-- | List of Fastly's public IP address ranges.
data Addresses = Addresses
  { addresses :: [IPRange]
  -- ^ IP ranges used by Fastly's network
  } deriving (Show)

instance FromJSON Addresses where
  parseJSON (Object o) = do
    r <- o .: "addresses"
    return $ Addresses $ map read r
  parseJSON wat = typeMismatch "Addresses" wat

-- ---------------------------------------------------------------------------
-- Utility Types
-- ---------------------------------------------------------------------------

-- | UTC timestamp wrapper.
--
-- Handles parsing of the various timestamp formats used by the Fastly API.
newtype Timestamp = Timestamp { fromTimestamp :: UTCTime }
  deriving (Show, Eq)

instance ToJSON Timestamp where
  toJSON (Timestamp t) = String $ pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" t

instance FromJSON Timestamp where
  parseJSON (String s) = let ts = unpack s in
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" ts <|>
         parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" ts <|>
         parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" ts of
      Nothing -> fail "Invalid timestamp format"
      Just t -> return $ Timestamp t
  parseJSON wat = typeMismatch "Timestamp" wat

-- | Boolean wrapper for JSON fields that are represented as "0" or "1" strings.
newtype Boolean = Boolean Bool
  deriving (Show, Eq)

instance ToJSON Boolean where
  toJSON (Boolean b) = String $ if b then "1" else "0"

instance FromJSON Boolean where
  parseJSON (String "0") = return $ Boolean False
  parseJSON (String "1") = return $ Boolean True
  parseJSON (Bool b)     = return $ Boolean b
  parseJSON wat          = typeMismatch "Boolean" wat
