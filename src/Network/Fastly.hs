{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Fastly
    ( FastlyAuth(..)
    , Username(..)
    , Password(..)
    , FastlyClient
    , fastlyClient
    , fastly
    , FastlyResponse(..)
      -- Account
    {-
    -}
      -- Configuration
    {-
    , listBackends
    , getBackend
    , createBackend
    , updateBackend
    , deleteBackend
    -}
    {-
    , listCacheSettings
    , getCacheSettings
    , createCacheSettings
    , updateCacheSettings
    , deleteCacheSettings
    -}
    {-
    , listConditions
    , getCondition
    , createCondition
    , updateCondition
    , deleteCondition
    -}
    -- Dictionaries
    , listDictionaries
    , getDictionary
    , createDictionary
    , updateDictionary
    , deleteDictionary
    -- Dictionary items
    , listDictionaryItems
    , getDictionaryItem
    , createDictionaryItem
    , upsertDictionaryItem
    , updateDictionaryItem
    , batchEditDictionaryItems
    , deleteDictionaryItem
    {-
    , getDiff
    -}
    {-
    , listDirectors
    , getDirector
    , createDirector
    , updateDirector
    , deleteDirector
    -}
    {-
    , getDirectorBackendRelationship
    , establishDirectorBackendRelationship
    , deleteDirectorBackendRelationship
    -}
    , checkDomainRecords
    , checkDomainRecord
    , listDomains
    , getDomain
    , createDomain
    {-
    , updateDomain
    , deleteDomain
    -}
    , listGzipConfigurations
    , getGzipConfiguration
    {-
    , createGzipConfiguration
    , updateGzipConfiguration
    -}
    , deleteGzipConfiguration
    {-
    , listHeaders
    , getHeader
    , createHeader
    , updateHeader
    , deleteHeader
    -}
    {-
    , listHealthchecks
    , getHealthcheck
    , createHealthcheck
    , updateHealthcheck
    , deleteHealthcheck
    -}
    {-
    , getRequestSettingsObject
    , listRequestSettingsObjects
    , createRequestSettingsObject
    , deleteRequestSettingsObject
    , updateRequestSettingsObject
    -}
    {-
    , getResponseObject
    , listResponseObjects
    , createResponseObject
    , deleteResponseObject
    , updateResponseObject
    -}
    {-
    -}
    , listServices
    , getServiceDetails
    , getServiceByName
    , getService
    , ServiceId(..)
    , Service(..)
    {-
    , createService
    , deleteService
    , updateService
    , listServiceDomains
    -}
    {-
    , getServiceSettings
    , updateServiceSettings
    -}
    {-
    , getServiceStats
    -}
    {-
    , getVcls
    , getBoilerplateVcl
    , getVcl
    , getSyntaxHighlightedVcl
    , downloadVcl -- ????
    , getGeneratedVcl
    , getSyntaxHighlightedGeneratedVcl
    , uploadVcl
    , deleteVcl
    , setMainVcl
    , updateVcl
    -}
    {-
    , getServiceVersion
    , createServiceVersion
    , listServiceVersions
    , updateServiceVersion
    , activateServiceVersion
    , deactivateServiceVersion
    , cloneServiceVersion
    , validateServiceVersion
    , lockServiceVersion
    -}
    {-
    , getWordpressConfiguration
    , listWordpressConfigurations
    , createWordpressConfiguration
    , deleteWordpressConfiguration
    , updateWordpressConfiguration
    -}
      -- Purging
    , PurgeMode(..)
    , SurrogateKey(..)
    , purge
    , purgeKey
    , purgeAll
    , PurgeResult(..)
    , PurgeAllResult(..)
      -- Historical Stats

      -- Remote Logging

      -- Miscellaneous
    , edgeCheck
    , CacheStatus(..)
    , CacheStatusRequest(..)
    , CacheStatusResponse(..)
    , publicIpList
    , Addresses(..)
    ) where
import Control.Applicative
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Types
import Data.IP
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lex.Integral (readDecimal_)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Thyme.Clock (UTCTime)
import Data.Thyme.Format
import Data.HashMap.Strict (HashMap)
import GHC.Generics
import Network.HTTP.Client as C
import qualified Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import System.IO.Unsafe
import System.Locale

data FastlyResponse a = FastlyResponse
                        { fastlyResponseRateLimitRemaining :: Int
                        , fastlyResponseRateLimitReset     :: Int -- ^ UTC epoch seconds
                        , fastlyResponseValue              :: a
                        } deriving (Show)

data FastlyClient = FastlyClient
  { fastlyClientBaseRequest   :: Request
  }

data FastlyError = InvalidOrMissingRateLimitData
                 | JsonError String
                 | InvalidUrl String
                 deriving (Show)

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

fastlyRootRequest :: Request
fastlyRootRequest = parseRequest_ "https://api.fastly.com/"

newtype Username = Username Text
newtype Password = Password Text

data FastlyAuth = AuthToken Text
                | Login Username Password

fastlyClient :: FastlyAuth -> IO FastlyClient
fastlyClient (AuthToken t) = do
  return $
    FastlyClient
      (fastlyRootRequest
       { requestHeaders =
           ("Fastly-Key", encodeUtf8 t) :
           ("Accept", "application/json") : requestHeaders fastlyRootRequest
       })
fastlyClient (Login (Username u) (Password p)) = do
  error "Login not supported yet"
  {-
  m <- newManager
  let req' = fastlyRootRequest { cookieJar = Just mempty
                               , path = "/login"
                               }
  let reqEditor r = case cookieJar r of
  resp <- httpNoBody req'' m
  reqRef <- newIORef $ fastlyRootRequest { cookieJar = responseCookieJar resp }
  return $ FastlyClient m reqRef
  -}

type FastlyM a = ExceptT FastlyError IO a

fastly :: Text -> (FastlyClient -> FastlyM a) -> IO (Either FastlyError a)
fastly t f = do
  c <- fastlyClient $ AuthToken t
  let (ExceptT m) = f c
  m


jsonOpts :: Int -> Options
jsonOpts x = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop x }

get :: FromJSON a => FastlyClient -> (Request -> Request) -> FastlyM a
get c f = ExceptT $ do
  m <- getGlobalManager
  r <- httpLbs ((f $ fastlyClientBaseRequest c) { method = "GET" }) m
  return $ case eitherDecode $ responseBody r of
             Left e -> Left $ JsonError e
             Right r -> Right r

post :: FromJSON a => FastlyClient -> (Request -> Request) -> FastlyM a
post c f = ExceptT $ do
  m <- getGlobalManager
  r <- httpLbs ((f $ fastlyClientBaseRequest c) { method = "POST" }) m
  return $ case eitherDecode $ responseBody r of
             Left e -> Left $ JsonError e
             Right r -> Right r

put :: FromJSON a => FastlyClient -> (Request -> Request) -> FastlyM a
put c f = ExceptT $ do
  m <- getGlobalManager
  r <- httpLbs ((f $ fastlyClientBaseRequest c) { method = "PUT" }) m
  return $ case eitherDecode $ responseBody r of
             Left e -> Left $ JsonError e
             Right r -> Right r

delete :: FromJSON a => FastlyClient -> (Request -> Request) -> FastlyM a
delete c f = ExceptT $ do
  m <- getGlobalManager
  r <- httpLbs ((f $ fastlyClientBaseRequest c) { method = "DELETE" }) m
  return $ case eitherDecode $ responseBody r of
             Left e -> Left $ JsonError e
             Right r -> Right r

patch :: FromJSON a => FastlyClient -> (Request -> Request) -> FastlyM a
patch c f = ExceptT $ do
  m <- getGlobalManager
  r <- httpLbs ((f $ fastlyClientBaseRequest c) { method = "PATCH" }) m
  return $ case eitherDecode $ responseBody r of
             Left e -> Left $ JsonError e
             Right r -> Right r

newtype DictionaryId = DictionaryId Text
                     deriving (Show, ToJSON, FromJSON)

data Dictionary = Dictionary
                  { dictionaryCreatedAt :: Timestamp
                  , dictionaryDeletedAt :: Maybe Timestamp
                  , dictionaryId        :: DictionaryId
                  , dictionaryName      :: Text
                  , dictionaryServiceId :: ServiceId
                  , dictionaryUpdatedAt :: Maybe Timestamp
                  , dictionaryVersion   :: ServiceVersionNumber
                  } deriving (Show, Generic)

instance ToJSON Dictionary where
  toJSON = genericToJSON (jsonOpts 10)

instance FromJSON Dictionary where
  parseJSON = genericParseJSON (jsonOpts 10)

listDictionaries :: FastlyClient
                 -> ServiceId
                 -> ServiceVersionNumber
                 -> FastlyM [Dictionary]
listDictionaries c (ServiceId sid) (ServiceVersionNumber v) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/dictionary" }

getDictionary :: FastlyClient
              -> ServiceId
              -> ServiceVersionNumber
              -> Text -- ^ Dictionary name
              -> FastlyM Dictionary
getDictionary c (ServiceId sid) (ServiceVersionNumber v) d = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/dictionary/" <> urlEncode False (encodeUtf8 d) }

createDictionary :: FastlyClient
                 -> ServiceId
                 -> ServiceVersionNumber
                 -> Text -- ^ Dictionary name
                 -> FastlyM Dictionary
createDictionary c (ServiceId sid) (ServiceVersionNumber v) d = post c $ \r -> urlEncodedBody [("name", encodeUtf8 d)] $
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/dictionary"
    }

updateDictionary :: FastlyClient
                 -> ServiceId
                 -> ServiceVersionNumber
                 -> Text
                 -> Text
                 -> FastlyM Dictionary
updateDictionary c (ServiceId sid) (ServiceVersionNumber v) dOld dNew = put c $ \r -> urlEncodedBody [("name", encodeUtf8 dNew)] $
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/dictionary/" <> urlEncode False (encodeUtf8 dOld) }


data DeleteDictionaryResult = DeleteDictionaryResult
                              { deleteDictionaryResultStatus :: Text
                              } deriving (Show, Generic)

instance ToJSON DeleteDictionaryResult where
  toJSON = genericToJSON (jsonOpts 22)

instance FromJSON DeleteDictionaryResult where
  parseJSON = genericParseJSON (jsonOpts 22)

deleteDictionary :: FastlyClient
                 -> ServiceId
                 -> ServiceVersionNumber
                 -> Text -- ^ Dictionary name
                 -> FastlyM Dictionary
deleteDictionary c (ServiceId sid) (ServiceVersionNumber v) d = delete c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/dictionary/" <> urlEncode False (encodeUtf8 d) }

data DictionaryItem = DictionaryItem
                      { dictionaryItemDictionaryId :: DictionaryId
                      , dictionaryItemServiceId    :: ServiceId
                      , dictionaryItemItemKey      :: Text
                      , dictionaryItemItemValue    :: Text
                      , dictionaryItemCreatedAt    :: Timestamp
                      , dictionaryItemUpdatedAt    :: Timestamp
                      , dictionaryItemDeletedAt    :: Maybe Timestamp
                      } deriving (Show, Generic)

instance ToJSON DictionaryItem where
  toJSON = genericToJSON (jsonOpts 14)

instance FromJSON DictionaryItem where
  parseJSON = genericParseJSON (jsonOpts 14)

listDictionaryItems :: FastlyClient
                    -> ServiceId
                    -> DictionaryId
                    -> FastlyM [DictionaryItem]
listDictionaryItems c (ServiceId s) (DictionaryId d) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 s <> "/dictionary/" <> encodeUtf8 d <> "/items" }

getDictionaryItem :: FastlyClient
                  -> ServiceId
                  -> DictionaryId
                  -> Text -- ^ Key
                  -> FastlyM DictionaryItem
getDictionaryItem c (ServiceId s) (DictionaryId d) k = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 s <> "/dictionary/" <> encodeUtf8 d <> "/item/" <> urlEncode False (encodeUtf8 k)}

createDictionaryItem :: FastlyClient
                     -> ServiceId
                     -> DictionaryId
                     -> Text -- ^ Key
                     -> Text -- ^ Value
                     -> FastlyM DictionaryItem
createDictionaryItem c (ServiceId s) (DictionaryId d) k v = post c $ \r -> urlEncodedBody [("item_key", encodeUtf8 k), ("item_value", encodeUtf8 v)] $
  r { requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : requestHeaders r
    , path = "/service/" <> encodeUtf8 s <> "/dictionary/" <> encodeUtf8 d <> "/item"
    }

upsertDictionaryItem :: FastlyClient
                     -> ServiceId
                     -> DictionaryId
                     -> Text -- ^ Key
                     -> Text -- ^ Value
                     -> FastlyM DictionaryItem
upsertDictionaryItem c (ServiceId s) (DictionaryId d) k v = put c $ \r -> urlEncodedBody [("item_value", encodeUtf8 v)] $
  r { requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : requestHeaders r
    , path = "/service/" <> encodeUtf8 s <> "/dictionary/" <> encodeUtf8 d <> "/item/" <> urlEncode False (encodeUtf8 k)
    }

updateDictionaryItem :: FastlyClient
                     -> ServiceId
                     -> DictionaryId
                     -> Text -- ^ Key
                     -> Text -- ^ Value
                     -> FastlyM DictionaryItem
updateDictionaryItem c (ServiceId s) (DictionaryId d) k v = put c $ \r -> urlEncodedBody [("item_value", encodeUtf8 v)] $
  r { requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : requestHeaders r
    , path = "/service/" <> encodeUtf8 s <> "/dictionary/" <> encodeUtf8 d <> "/item/" <> urlEncode False (encodeUtf8 k)
    }

data DictionaryItemOp = CreateItem { dictionaryOpKey :: Text, dictionaryOpValue :: Text }
                      | UpdateItem { dictionaryOpKey :: Text, dictionaryOpValue :: Text }
                      | UpsertItem { dictionaryOpKey :: Text, dictionaryOpValue :: Text }
                      | DeleteItem { dictionaryOpKey :: Text }

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

data BatchEditResult = BatchEditResult
                       { batchEditResultStatus :: Text
                       } deriving (Show, Generic)

instance ToJSON BatchEditResult where
  toJSON = genericToJSON (jsonOpts 15)

instance FromJSON BatchEditResult where
  parseJSON = genericParseJSON (jsonOpts 15)

batchEditDictionaryItems :: FastlyClient
                         -> ServiceId
                         -> DictionaryId
                         -> [DictionaryItemOp]
                         -> FastlyM BatchEditResult
batchEditDictionaryItems c (ServiceId s) (DictionaryId d) ops = patch c $ \r ->
  r { path = "/service/" <> encodeUtf8 s <> "/dictionary/" <> encodeUtf8 d <> "/items"
    , requestBody = RequestBodyLBS $ encode $ object [ "items" .= ops ]
    }

data DeleteDictionaryItemResult = DeleteDictionaryItemResult
                              { deleteDictionaryItemResultStatus :: Text
                              } deriving (Show, Generic)

instance ToJSON DeleteDictionaryItemResult where
  toJSON = genericToJSON (jsonOpts 26)

instance FromJSON DeleteDictionaryItemResult where
  parseJSON = genericParseJSON (jsonOpts 26)

deleteDictionaryItem :: FastlyClient
                     -> ServiceId
                     -> DictionaryId
                     -> Text -- ^ Key
                     -> FastlyM DeleteDictionaryItemResult
deleteDictionaryItem c (ServiceId s) (DictionaryId d) k = delete c $ \r ->
  r { path = "/service/" <> encodeUtf8 s <> "/dictionary/" <> encodeUtf8 d <> "/item/" <> urlEncode False (encodeUtf8 k) }

data Domain = Domain
  { domainComment   :: Text
  , domainLocked    :: Bool
  , domainName      :: Text
  , domainServiceId :: ServiceId
  , domainVersion   :: ServiceVersionNumber
  , domainUpdatedAt :: Maybe Timestamp
  , domainDeletedAt :: Maybe Timestamp
  , domainCreatedAt :: Maybe Timestamp
  } deriving (Show, Generic)

instance ToJSON Domain where
  toJSON = genericToJSON (jsonOpts 6)

instance FromJSON Domain where
  parseJSON = genericParseJSON (jsonOpts 6)

checkDomainRecords
  :: FastlyClient
  -> ServiceId
  -> ServiceVersionNumber
  -> FastlyM [(Domain, Text, Bool)]
checkDomainRecords c (ServiceId s) (ServiceVersionNumber v) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 s <> "/version/" <> BS.pack (show v) <> "/domain/check_all" }

checkDomainRecord
  :: FastlyClient
  -> ServiceId
  -> ServiceVersionNumber
  -> Text
  -> FastlyM (Domain, Text, Bool)
checkDomainRecord c (ServiceId s) (ServiceVersionNumber v) n = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 s <> "/version/" <> BS.pack (show v) <> "/domain/" <> urlEncode False (encodeUtf8 n) <> "/check" }

listDomains
  :: FastlyClient
  -> ServiceId
  -> ServiceVersionNumber
  -> FastlyM [Domain]
listDomains c (ServiceId s) (ServiceVersionNumber v) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 s <> "/version/" <> BS.pack (show v) <> "/domain" }

getDomain
  :: FastlyClient
  -> ServiceId
  -> ServiceVersionNumber
  -> Text
  -> FastlyM Domain
getDomain c (ServiceId s) (ServiceVersionNumber v) n = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 s <> "/version/" <> BS.pack (show v) <> "/domain/" <> urlEncode False (encodeUtf8 n) }

createDomain
  :: FastlyClient
  -> ServiceId
  -> ServiceVersionNumber
  -> Text -- ^ Domain name
  -> FastlyM Domain
createDomain c (ServiceId s) (ServiceVersionNumber v) n = post c $ \r -> urlEncodedBody [("name", encodeUtf8 n)] $
  r { requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : requestHeaders r
    , path = "/service/" <> encodeUtf8 s <> "/version/" <> BS.pack (show v) <> "/domain"
    }



toJsonSpaceList :: [Text] -> Value
toJsonSpaceList = String . T.intercalate " "

fromJsonSpaceList :: Monad m => Value -> m [Text]
fromJsonSpaceList (String ts) = return $ T.splitOn " " ts
fromJsonSpaceList wat = fail "Expected a String, got something else for JSON space-delimited list"

newtype ContentTypes = ContentTypes [Text]
                     deriving (Show)

instance ToJSON ContentTypes where
  toJSON (ContentTypes ts) = toJsonSpaceList ts

instance FromJSON ContentTypes where
  parseJSON ts = ContentTypes <$> fromJsonSpaceList ts

newtype Extensions = Extensions [Text]
                   deriving (Show)

instance ToJSON Extensions where
  toJSON (Extensions ts) = toJsonSpaceList ts

instance FromJSON Extensions where
  parseJSON ts = Extensions <$> fromJsonSpaceList ts

data GzipConfiguration = GzipConfiguration
                         { gzipConfigurationCacheCondition :: Text
                         , gzipConfigurationContentTypes   :: ContentTypes
                         , gzipConfigurationExtensions     :: Extensions
                         , gzipConfigurationName           :: Text
                         , gzipConfigurationServiceId      :: ServiceId
                         , gzipConfigurationVersion        :: ServiceVersionNumber
                         } deriving (Show, Generic)

instance ToJSON GzipConfiguration where
  toJSON = genericToJSON (jsonOpts 17)

instance FromJSON GzipConfiguration where
  parseJSON = genericParseJSON (jsonOpts 17)

listGzipConfigurations :: FastlyClient
                       -> ServiceId
                       -> ServiceVersionNumber
                       -> FastlyM [GzipConfiguration]
listGzipConfigurations c (ServiceId s) (ServiceVersionNumber v) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 s <> "/version/" <> BS.pack (show v) <> "/gzip" }

getGzipConfiguration :: FastlyClient
                     -> ServiceId
                     -> ServiceVersionNumber
                     -> Text -- ^ Configuration name
                     -> FastlyM GzipConfiguration
getGzipConfiguration c (ServiceId s) (ServiceVersionNumber v) n = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 s <> "/version/" <> BS.pack (show v) <> "/gzip/" <> urlEncode False (encodeUtf8 n) }

data DeleteGzipConfigurationResult = DeleteGzipConfigurationResult
                                     { deleteGzipConfigurationResultStatus :: Text
                                     } deriving (Show, Generic)

instance ToJSON DeleteGzipConfigurationResult where
  toJSON = genericToJSON (jsonOpts 29)

instance FromJSON DeleteGzipConfigurationResult where
  parseJSON = genericParseJSON (jsonOpts 29)

deleteGzipConfiguration :: FastlyClient
                        -> ServiceId
                        -> ServiceVersionNumber
                        -> Text -- ^ Configuration name
                        -> FastlyM DeleteGzipConfigurationResult
deleteGzipConfiguration c (ServiceId s) (ServiceVersionNumber v) n = delete c $ \r ->
  r { path = "/service/" <> encodeUtf8 s <> "/version/" <> BS.pack (show v) <> "/gzip/" <> urlEncode False (encodeUtf8 n) }


data HeaderAction = SetHeader
                  | AppendHeader
                  | DeleteHeader
                  | RegexHeader
                  | RegexRepeatHeader

data HeaderType = Request
                | Fetch
                | Cache
                | Response

data Header = Header
              { headerAction            :: HeaderAction
              , headerCacheCondition    :: Maybe Text
              , headerDst               :: Text
              , headerIgnoreIfSet       :: Int -- ^ TODO it's actually a bool, except for JSON format
              , headerName              :: Text
              , headerPriority          :: Int
              , headerRegex             :: Text
              , headerRequestCondition  :: Text
              , headerResponseCondition :: Text
              , headerServiceId         :: ServiceId
              , headerSrc               :: Text
              , headerSubstitution      :: Text
              , headerType              :: HeaderType
              , headerVersion           :: ServiceVersionNumber
              }





newtype ServiceVersionNumber = ServiceVersionNumber Int
                       deriving (Show, ToJSON)

instance FromJSON ServiceVersionNumber where
  parseJSON (String str) = return $ ServiceVersionNumber $ read $ unpack str
  parseJSON num@(Number n) = fmap ServiceVersionNumber $ parseJSON num
  parseJSON wat = typeMismatch "ServiceVersion" wat

newtype ServiceId = ServiceId Text
                  deriving (Show, ToJSON, FromJSON)

newtype CustomerId = CustomerId Text
                   deriving (Show, ToJSON, FromJSON)

newtype Timestamp = Timestamp { fromTimestamp :: UTCTime }
                    deriving (Show)

instance ToJSON Timestamp where
  toJSON (Timestamp t) = String $ pack $ formatTime defaultTimeLocale "%Y-%M-%dT%H:%M:%S" t

instance FromJSON Timestamp where
  parseJSON (String s) = case parseTime defaultTimeLocale "%Y-%M-%dT%H:%M:%S" (unpack s) <|> parseTime defaultTimeLocale "%Y-%M-%dT%H:%M:%S%z" (unpack s) of
    Nothing -> fail "Invalid time parse"
    Just t -> return $ Timestamp t
  parseJSON wat = typeMismatch "Timestamp" wat

newtype Boolean = Boolean Bool
                deriving (Show)

instance ToJSON Boolean where
  toJSON (Boolean b) = String $ if b then "1" else "0"

instance FromJSON Boolean where
  parseJSON (String s) = case s of
    "0" -> return $ Boolean False
    "1" -> return $ Boolean True
  parseJSON (Bool b) = return $ Boolean b

data ServiceBasicVersion = ServiceBasicVersion
                           { serviceBasicVersionDeployed         :: Value
                           , serviceBasicVersionLocked           :: Maybe Boolean
                           , serviceBasicVersionTesting          :: Maybe Boolean
                           , serviceBasicVersionActive           :: Value
                           , serviceBasicVersionUpdatedAt        :: Maybe Timestamp
                           , serviceBasicVersionDeletedAt        :: Maybe Timestamp
                           , serviceBasicVersionServiceId        :: ServiceId
                           , serviceBasicVersionInheritServiceId :: Maybe ServiceId
                           , serviceBasicVersionCreatedAt        :: Maybe Timestamp
                           , serviceBasicVersionNumber           :: ServiceVersionNumber
                           , serviceBasicVersionComment          :: Text
                           , serviceBasicVersionStaging          :: Value
                           } deriving (Show, Generic)

instance ToJSON ServiceBasicVersion where
  toJSON = genericToJSON (jsonOpts 19)

instance FromJSON ServiceBasicVersion where
  parseJSON = genericParseJSON (jsonOpts 19)

data ServiceVersion = ServiceVersion
                      { serviceVersionBackends        :: [Object]
                      , serviceVersionGzips           :: [Object]
                      , serviceVersionDeployed        :: Value
                      , serviceVersionVcls            :: [Value]
                      , serviceVersionLocked          :: Maybe Boolean
                      , serviceVersionSettings        :: Object
                      , serviceVersionMatches         :: [Value]
                      , serviceVersionTesting         :: Value
                      , serviceVersionActive          :: Value
                      , serviceVersionHeaders         :: [Object]
                      , serviceVersionServiceId       :: ServiceId
                      , serviceVersionResponseObjects :: [Object]
                      , serviceVersionConditions      :: [Object]
                      , serviceVersionNumber          :: ServiceVersionNumber
                      , serviceVersionDomains         :: [Object]
                      , serviceVersionRequestSettings :: [Object]
                      , serviceVersionComment         :: Text
                      , serviceVersionDirectors       :: [Object]
                      , serviceVersionCacheSettings   :: [Object]
                      , serviceVersionHealthchecks    :: [Object]
                      , serviceVersionStaging         :: Value
                      , serviceVersionWordpress       :: [Object]
                      } deriving (Show, Generic)

instance ToJSON ServiceVersion where
  toJSON = genericToJSON (jsonOpts 14)

instance FromJSON ServiceVersion where
  parseJSON = genericParseJSON (jsonOpts 14)

data Service = Service
               { serviceId            :: ServiceId
               , serviceName          :: Text
               , serviceCustomerId    :: CustomerId
               , serviceComment       :: Text
               , serviceActiveVersion :: Maybe ServiceVersion
               , serviceVersion       :: Maybe ServiceVersion
               , serviceVersions      :: [ServiceBasicVersion]
               } deriving (Show, Generic)

instance ToJSON Service where
  toJSON = genericToJSON (jsonOpts 7)

instance FromJSON Service where
  parseJSON = genericParseJSON (jsonOpts 7)

data ServiceListItem = ServiceListItem
                       { serviceListItemCustomerId :: CustomerId
                       , serviceListItemVersions   :: [ServiceBasicVersion]
                       , serviceListItemVersion    :: ServiceVersionNumber
                       , serviceListItemName       :: Text
                       , serviceListItemId         :: ServiceId
                       , serviceListItemComment    :: Text
                       } deriving (Show, Generic)

instance ToJSON ServiceListItem where
  toJSON = genericToJSON (jsonOpts 15)

instance FromJSON ServiceListItem where
  parseJSON = genericParseJSON (jsonOpts 15)

listServices :: FastlyClient
             -> FastlyM [ServiceListItem]
listServices c = get c $ \r ->
  r { path = "/service" }

getService :: FastlyClient
           -> ServiceId
           -> FastlyM Service
getService c (ServiceId sid) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid }

getServiceDetails :: FastlyClient
           -> ServiceId
           -> FastlyM Service
getServiceDetails c (ServiceId sid) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/details" }

getServiceByName :: FastlyClient
                 -> Text
                 -> FastlyM (Maybe Service)
getServiceByName c name = get c $ \r -> setQueryString [("name", Just $ encodeUtf8 name)] $
  r { path = "/service/search"
    }



data PurgeMode = Instant
               | Soft

data PurgeResult = PurgeResult
  { purgeResultStatus :: Text
  , purgeResultId     :: Text
  } deriving (Show, Generic)

instance ToJSON PurgeResult where
  toJSON = genericToJSON (jsonOpts 11)

instance FromJSON PurgeResult where
  parseJSON = genericParseJSON (jsonOpts 11)

purge :: FastlyClient
      -> PurgeMode
      -> String -- ^ URL
      -> FastlyM PurgeResult
purge c mode url = ExceptT $ do
  m <- getGlobalManager
  case parseRequest url of
    Left str -> return $ Left $ InvalidUrl $ show str
    Right req -> do
      let f r = case mode of
            Instant -> r
            Soft -> (r { requestHeaders = ("Fastly-Soft-Purge", "1") : requestHeaders r})
      r <- httpLbs ((f req) { method = "PURGE" }) m
      return $ case eitherDecode $ responseBody r of
                Left e -> Left $ JsonError e
                Right r -> Right r

newtype SurrogateKey = SurrogateKey Text

purgeKey :: FastlyClient
         -> PurgeMode
         -> ServiceId
         -> SurrogateKey
         -> FastlyM PurgeResult
purgeKey c mode (ServiceId sid) (SurrogateKey skey) = ExceptT $ do
  m <- getGlobalManager
  let baseReq = fastlyClientBaseRequest c
      req = baseReq { method = "POST"
                    , path = "/service/" <> encodeUtf8 sid <> "/purge/" <> encodeUtf8 skey
                    , requestHeaders = case mode of
                        Instant -> requestHeaders baseReq
                        Soft -> ("Fastly-Soft-Purge", "1") : requestHeaders baseReq
                    }
  r <- httpLbs req m
  return $ case eitherDecode $ responseBody r of
    Left e -> Left $ JsonError e
    Right r -> Right r

data PurgeAllResult = PurgeAllResult
                      { purgeAllResultStatus :: Text
                      } deriving (Show, Generic)

instance ToJSON PurgeAllResult where
  toJSON = genericToJSON (jsonOpts 14)

instance FromJSON PurgeAllResult where
  parseJSON = genericParseJSON (jsonOpts 14)

purgeAll :: FastlyClient
         -> ServiceId
         -> FastlyM PurgeAllResult
purgeAll c (ServiceId sid) = ExceptT $ do
  m <- getGlobalManager
  let req = (fastlyClientBaseRequest c) { method = "POST"
                                        , path = "/service/" <> encodeUtf8 sid <> "/purge_all"
                                        }
  r <- httpLbs req m
  return $ case eitherDecode $ responseBody r of
    Left e -> Left $ JsonError e
    Right r -> Right r











data CacheStatus = CacheStatus
  { cacheStatusHash         :: Text
  , cacheStatusResponse     :: CacheStatusResponse
  , cacheStatusResponseTime :: Double
  , cacheStatusServer       :: Text
  , cacheStatusRequest      :: CacheStatusRequest
  } deriving (Show, Generic)

instance ToJSON CacheStatus where
  toJSON = genericToJSON (jsonOpts 11)

instance FromJSON CacheStatus where
  parseJSON = genericParseJSON (jsonOpts 11)

data CacheStatusRequest = CacheStatusRequest
  { cacheStatusRequestUrl     :: Maybe Text
  , cacheStatusRequestHeaders :: HashMap Text Text
  , cacheStatusRequestMethod  :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON CacheStatusRequest where
  toJSON = genericToJSON (jsonOpts 18)

instance FromJSON CacheStatusRequest where
  parseJSON = genericParseJSON (jsonOpts 18)

data CacheStatusResponse = CacheStatusResponse
  { cacheStatusResponseStatus :: Int
  , cacheStatusResponseHeaders :: HashMap Text Text
  } deriving (Show, Generic)

instance ToJSON CacheStatusResponse where
  toJSON = genericToJSON (jsonOpts 19)

instance FromJSON CacheStatusResponse where
  parseJSON = genericParseJSON (jsonOpts 19)

edgeCheck :: FastlyClient -> Text -> FastlyM [CacheStatus]
edgeCheck c t = get c (p . setQueryString [("url", Just $ encodeUtf8 t)])
  where
    p r = r { path = "/content/edge_check" }

data Addresses = Addresses
                 { addresses :: [IPRange]
                 } deriving (Show)

instance FromJSON Addresses where
  parseJSON (Object o) = do
    r <- o .: "addresses"
    return $ Addresses $ map read r


publicIpList :: FastlyM Addresses
publicIpList = ExceptT $ do
  m <- getGlobalManager
  r <- httpLbs req m
  return $
    case eitherDecode $ responseBody r of
      Left e -> Left $ JsonError e
      Right r -> Right r
  where
    (Just req) = C.parseRequest "https://api.fastly.com/public-ip-list"
