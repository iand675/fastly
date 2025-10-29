{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly.Dictionary
Description : Edge Dictionary operations for the Fastly API
Copyright   : (c) 2018-2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module provides operations for managing Fastly Edge Dictionaries.

Edge Dictionaries are key-value stores that live at the edge and can be accessed
from VCL. They can be updated without deploying a new service version, making them
perfect for:

* Feature flags
* A/B testing configurations
* API keys and credentials (though consider using secret stores for sensitive data)
* IP allowlists/blocklists
* Dynamic routing rules

= Dictionary Workflow

1. Create a dictionary in a service version with 'createDictionary'
2. Activate the service version
3. Add, update, or delete items using the item operations
4. Items can be modified without creating a new service version

= Batch Operations

For bulk updates, use 'batchEditDictionaryItems' which is more efficient
than individual operations:

@
let ops = [ CreateItem "key1" "value1"
          , UpsertItem "key2" "value2"
          , DeleteItem "old-key"
          ]
result <- batchEditDictionaryItems client serviceId dictId ops
@
-}

module Network.Fastly.Dictionary
  ( -- * Dictionary Operations
    listDictionaries
  , getDictionary
  , createDictionary
  , updateDictionary
  , deleteDictionary

    -- * Dictionary Item Operations
  , listDictionaryItems
  , getDictionaryItem
  , createDictionaryItem
  , updateDictionaryItem
  , upsertDictionaryItem
  , deleteDictionaryItem

    -- * Batch Operations
  , batchEditDictionaryItems
  ) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Request(..), RequestBody(..), requestHeaders, urlEncodedBody)
import Network.HTTP.Types (urlEncode)

import Network.Fastly.Client
import Network.Fastly.Types

-- ---------------------------------------------------------------------------
-- Dictionary Operations
-- ---------------------------------------------------------------------------

-- | List all dictionaries for a service version.
--
-- Returns all edge dictionaries defined in the specified service version.
--
-- ==== __Examples__
--
-- @
-- dicts <- listDictionaries client serviceId versionNum
-- mapM_ (\\d -> putStrLn $ dictionaryName d) dicts
-- @
listDictionaries :: FastlyClient
                 -> ServiceId
                 -> ServiceVersionNumber
                 -> FastlyM [Dictionary]
listDictionaries c (ServiceId sid) (ServiceVersionNumber v) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/dictionary" }

-- | Get a specific dictionary by name.
--
-- ==== __Examples__
--
-- @
-- dict <- getDictionary client serviceId versionNum "my-dictionary"
-- putStrLn $ "Dictionary ID: " ++ show (dictionaryId dict)
-- @
getDictionary :: FastlyClient
              -> ServiceId
              -> ServiceVersionNumber
              -> Text  -- ^ Dictionary name
              -> FastlyM Dictionary
getDictionary c (ServiceId sid) (ServiceVersionNumber v) name = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/dictionary/" <> urlEncode False (encodeUtf8 name) }

-- | Create a new dictionary in a service version.
--
-- The dictionary will be created in the specified service version.
-- The version must not be active or locked.
--
-- After creating the dictionary and activating the version, you can add items
-- to it without creating a new version.
--
-- ==== __Examples__
--
-- @
-- dict <- createDictionary client serviceId versionNum "feature-flags"
-- putStrLn $ "Created dictionary: " ++ show (dictionaryId dict)
-- @
createDictionary :: FastlyClient
                 -> ServiceId
                 -> ServiceVersionNumber
                 -> Text  -- ^ Dictionary name
                 -> FastlyM Dictionary
createDictionary c (ServiceId sid) (ServiceVersionNumber v) name = post c $ \r -> urlEncodedBody [("name", encodeUtf8 name)] $
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/dictionary" }

-- | Update a dictionary's name.
--
-- Renames a dictionary. The version must not be active or locked.
--
-- ==== __Examples__
--
-- @
-- dict <- updateDictionary client serviceId versionNum "old-name" "new-name"
-- @
updateDictionary :: FastlyClient
                 -> ServiceId
                 -> ServiceVersionNumber
                 -> Text  -- ^ Current dictionary name
                 -> Text  -- ^ New dictionary name
                 -> FastlyM Dictionary
updateDictionary c (ServiceId sid) (ServiceVersionNumber v) oldName newName = put c $ \r -> urlEncodedBody [("name", encodeUtf8 newName)] $
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/dictionary/" <> urlEncode False (encodeUtf8 oldName) }

-- | Delete a dictionary from a service version.
--
-- The version must not be active or locked.
-- All items in the dictionary will also be deleted.
--
-- __Note:__ This removes the dictionary from the service configuration.
-- It does not delete the items themselves if the dictionary is in use
-- by an active version.
deleteDictionary :: FastlyClient
                 -> ServiceId
                 -> ServiceVersionNumber
                 -> Text  -- ^ Dictionary name
                 -> FastlyM Dictionary
deleteDictionary c (ServiceId sid) (ServiceVersionNumber v) name = delete c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/dictionary/" <> urlEncode False (encodeUtf8 name) }

-- ---------------------------------------------------------------------------
-- Dictionary Item Operations
-- ---------------------------------------------------------------------------

-- | List all items in a dictionary.
--
-- Returns all key-value pairs stored in the specified dictionary.
-- This operation works on active dictionaries.
--
-- ==== __Examples__
--
-- @
-- items <- listDictionaryItems client serviceId dictId
-- mapM_ (\\item -> putStrLn $ dictionaryItemItemKey item ++ " = " ++ dictionaryItemItemValue item) items
-- @
listDictionaryItems :: FastlyClient
                    -> ServiceId
                    -> DictionaryId
                    -> FastlyM [DictionaryItem]
listDictionaryItems c (ServiceId sid) (DictionaryId dictId) = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/dictionary/" <> encodeUtf8 dictId <> "/items" }

-- | Get a specific item from a dictionary by key.
--
-- ==== __Examples__
--
-- @
-- item <- getDictionaryItem client serviceId dictId "feature-enabled"
-- putStrLn $ "Value: " ++ dictionaryItemItemValue item
-- @
getDictionaryItem :: FastlyClient
                  -> ServiceId
                  -> DictionaryId
                  -> Text  -- ^ Item key
                  -> FastlyM DictionaryItem
getDictionaryItem c (ServiceId sid) (DictionaryId dictId) key = get c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/dictionary/" <> encodeUtf8 dictId <> "/item/" <> urlEncode False (encodeUtf8 key) }

-- | Create a new item in a dictionary.
--
-- Adds a new key-value pair to the dictionary. Fails if the key already exists.
-- Use 'upsertDictionaryItem' if you want to create or update.
--
-- This operation can be performed on active dictionaries without deploying
-- a new service version.
--
-- ==== __Examples__
--
-- @
-- item <- createDictionaryItem client serviceId dictId "new-feature" "enabled"
-- @
createDictionaryItem :: FastlyClient
                     -> ServiceId
                     -> DictionaryId
                     -> Text  -- ^ Item key
                     -> Text  -- ^ Item value
                     -> FastlyM DictionaryItem
createDictionaryItem c (ServiceId sid) (DictionaryId dictId) key value = post c $ \r -> urlEncodedBody [("item_key", encodeUtf8 key), ("item_value", encodeUtf8 value)] $
  r { requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : requestHeaders r
    , path = "/service/" <> encodeUtf8 sid <> "/dictionary/" <> encodeUtf8 dictId <> "/item"
    }

-- | Update an existing item in a dictionary.
--
-- Updates the value of an existing key. Fails if the key doesn't exist.
-- Use 'upsertDictionaryItem' if you want to create or update.
--
-- ==== __Examples__
--
-- @
-- item <- updateDictionaryItem client serviceId dictId "feature-flag" "disabled"
-- @
updateDictionaryItem :: FastlyClient
                     -> ServiceId
                     -> DictionaryId
                     -> Text  -- ^ Item key
                     -> Text  -- ^ New item value
                     -> FastlyM DictionaryItem
updateDictionaryItem c (ServiceId sid) (DictionaryId dictId) key value = put c $ \r -> urlEncodedBody [("item_value", encodeUtf8 value)] $
  r { requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : requestHeaders r
    , path = "/service/" <> encodeUtf8 sid <> "/dictionary/" <> encodeUtf8 dictId <> "/item/" <> urlEncode False (encodeUtf8 key)
    }

-- | Create or update an item in a dictionary.
--
-- This is the recommended way to set dictionary values as it works
-- regardless of whether the key exists or not.
--
-- ==== __Examples__
--
-- @
-- -- This works whether the key exists or not
-- item <- upsertDictionaryItem client serviceId dictId "config-value" "42"
-- @
upsertDictionaryItem :: FastlyClient
                     -> ServiceId
                     -> DictionaryId
                     -> Text  -- ^ Item key
                     -> Text  -- ^ Item value
                     -> FastlyM DictionaryItem
upsertDictionaryItem c (ServiceId sid) (DictionaryId dictId) key value = put c $ \r -> urlEncodedBody [("item_value", encodeUtf8 value)] $
  r { requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : requestHeaders r
    , path = "/service/" <> encodeUtf8 sid <> "/dictionary/" <> encodeUtf8 dictId <> "/item/" <> urlEncode False (encodeUtf8 key)
    }

-- | Delete an item from a dictionary.
--
-- Removes the specified key from the dictionary.
-- This operation can be performed on active dictionaries.
--
-- ==== __Examples__
--
-- @
-- result <- deleteDictionaryItem client serviceId dictId "old-config"
-- putStrLn $ deleteDictionaryItemResultStatus result
-- @
deleteDictionaryItem :: FastlyClient
                     -> ServiceId
                     -> DictionaryId
                     -> Text  -- ^ Item key
                     -> FastlyM DeleteDictionaryItemResult
deleteDictionaryItem c (ServiceId sid) (DictionaryId dictId) key = delete c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/dictionary/" <> encodeUtf8 dictId <> "/item/" <> urlEncode False (encodeUtf8 key) }

-- ---------------------------------------------------------------------------
-- Batch Operations
-- ---------------------------------------------------------------------------

-- | Perform multiple dictionary operations in a single request.
--
-- This is much more efficient than making individual requests when you need
-- to create, update, or delete multiple items. The operations are applied atomically.
--
-- Supported operations:
--
-- * 'CreateItem': Create a new item (fails if exists)
-- * 'UpdateItem': Update an existing item (fails if doesn't exist)
-- * 'UpsertItem': Create or update an item
-- * 'DeleteItem': Delete an item
--
-- ==== __Examples__
--
-- @
-- let ops = [ UpsertItem "feature-a" "enabled"
--           , UpsertItem "feature-b" "disabled"
--           , DeleteItem "old-feature"
--           , CreateItem "new-feature" "testing"
--           ]
-- result <- batchEditDictionaryItems client serviceId dictId ops
-- putStrLn $ "Batch edit status: " ++ batchEditResultStatus result
-- @
batchEditDictionaryItems :: FastlyClient
                         -> ServiceId
                         -> DictionaryId
                         -> [DictionaryItemOp]  -- ^ List of operations to perform
                         -> FastlyM BatchEditResult
batchEditDictionaryItems c (ServiceId sid) (DictionaryId dictId) ops = patch c $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/dictionary/" <> encodeUtf8 dictId <> "/items"
    , requestBody = RequestBodyLBS $ encode $ object [ "items" .= ops ]
    }
