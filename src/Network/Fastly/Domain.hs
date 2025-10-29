{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly.Domain
Description : Domain management operations for the Fastly API
Copyright   : (c) 2018-2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module provides operations for managing domains associated with Fastly services.

Domains define which hostnames a service will handle. When a request arrives at Fastly's
edge servers, the @Host@ header is used to determine which service configuration to apply.

= Domain Management Workflow

1. Create a draft service version or clone an existing one
2. Add domains using 'createDomain'
3. Configure DNS to point your domains to Fastly (verify with 'checkDomainRecord')
4. Validate and activate the service version
5. Traffic for those domains will now be handled by your service

= DNS Configuration

To use a domain with Fastly, you typically need to:

* Create a CNAME record pointing to your Fastly service URL
* Or configure A/AAAA records to point to Fastly's IP addresses

Use 'checkDomainRecord' or 'checkDomainRecords' to verify DNS is configured correctly.

= Examples

@
-- Add a domain to a service version
domain <- createDomain client serviceId versionNum "www.example.com"

-- Check if DNS is configured correctly
(domain, _cname, isValid) <- checkDomainRecord client serviceId versionNum "www.example.com"
when isValid $ putStrLn "DNS is configured correctly!"

-- List all domains for a version
domains <- listDomains client serviceId versionNum
@
-}

module Network.Fastly.Domain
  ( -- * Domain Operations
    listDomains
  , getDomain
  , createDomain
  , updateDomain
  , deleteDomain

    -- * DNS Verification
  , checkDomainRecord
  , checkDomainRecords
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Request(..), urlEncodedBody, requestHeaders)
import Network.HTTP.Types (urlEncode)

import Network.Fastly.Client (MonadFastly(..))
import Network.Fastly.Types

-- ---------------------------------------------------------------------------
-- Domain Operations
-- ---------------------------------------------------------------------------

-- | List all domains for a service version.
--
-- Returns all domains configured in the specified service version.
--
-- ==== __Examples__
--
-- @
-- domains <- listDomains client serviceId versionNum
-- mapM_ (\\d -> putStrLn $ domainName d) domains
-- @
listDomains :: MonadFastly m =>
            ServiceId
            -> ServiceVersionNumber
            -> m [Domain]
listDomains (ServiceId sid) (ServiceVersionNumber v) = fastlyGet $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/domain" }

-- | Get details about a specific domain.
--
-- ==== __Examples__
--
-- @
-- domain <- getDomain client serviceId versionNum "www.example.com"
-- putStrLn $ "Domain comment: " ++ domainComment domain
-- @
getDomain :: MonadFastly m =>
          ServiceId
          -> ServiceVersionNumber
          -> Text  -- ^ Domain name
          -> m Domain
getDomain (ServiceId sid) (ServiceVersionNumber v) name = fastlyGet $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/domain/" <> urlEncode False (encodeUtf8 name) }

-- | Add a domain to a service version.
--
-- The domain will be configured to be served by this service.
-- The service version must not be active or locked.
--
-- After adding the domain and activating the version, you'll need to configure
-- DNS to point the domain to Fastly. Use 'checkDomainRecord' to verify DNS configuration.
--
-- ==== __Examples__
--
-- @
-- domain <- createDomain client serviceId versionNum "www.example.com"
-- putStrLn $ "Added domain: " ++ domainName domain
-- @
createDomain :: MonadFastly m =>
             ServiceId
             -> ServiceVersionNumber
             -> Text  -- ^ Domain name (e.g., "www.example.com")
             -> m Domain
createDomain (ServiceId sid) (ServiceVersionNumber v) name = fastlyPost $ \r -> urlEncodedBody [("name", encodeUtf8 name)] $
  r { requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : requestHeaders r
    , path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/domain"
    }

-- | Update a domain's properties.
--
-- You can update the comment associated with a domain.
-- The service version must not be active or locked.
--
-- ==== __Examples__
--
-- @
-- domain <- updateDomain client serviceId versionNum "www.example.com" (Just "Production domain")
-- @
updateDomain :: MonadFastly m =>
             ServiceId
             -> ServiceVersionNumber
             -> Text  -- ^ Domain name
             -> Maybe Text  -- ^ New comment
             -> m Domain
updateDomain (ServiceId sid) (ServiceVersionNumber v) name comment = fastlyPut $ \r -> urlEncodedBody params $
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/domain/" <> urlEncode False (encodeUtf8 name) }
  where
    params = maybe [] (\c' -> [("comment", encodeUtf8 c')]) comment

-- | Remove a domain from a service version.
--
-- The service version must not be active or locked.
--
-- ==== __Examples__
--
-- @
-- domain <- deleteDomain client serviceId versionNum "old.example.com"
-- @
deleteDomain :: MonadFastly m =>
             ServiceId
             -> ServiceVersionNumber
             -> Text  -- ^ Domain name
             -> m Domain
deleteDomain (ServiceId sid) (ServiceVersionNumber v) name = fastlyDelete $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/domain/" <> urlEncode False (encodeUtf8 name) }

-- ---------------------------------------------------------------------------
-- DNS Verification Operations
-- ---------------------------------------------------------------------------

-- | Check if a domain's DNS is correctly configured to point to Fastly.
--
-- This verifies that the domain's DNS records are set up correctly to route
-- traffic through Fastly. Returns:
--
-- * The domain object
-- * The expected CNAME target
-- * A boolean indicating whether DNS is configured correctly
--
-- ==== __Examples__
--
-- @
-- (domain, cname, isValid) <- checkDomainRecord client serviceId versionNum "www.example.com"
-- if isValid
--   then putStrLn "DNS is configured correctly"
--   else putStrLn $ "Please configure DNS CNAME to: " ++ cname
-- @
checkDomainRecord :: MonadFastly m =>
                  ServiceId
                  -> ServiceVersionNumber
                  -> Text  -- ^ Domain name to check
                  -> m (Domain, Text, Bool)
checkDomainRecord (ServiceId sid) (ServiceVersionNumber v) name = fastlyGet $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/domain/" <> urlEncode False (encodeUtf8 name) <> "/check" }

-- | Check DNS configuration for all domains in a service version.
--
-- This is a bulk version of 'checkDomainRecord' that checks all domains
-- at once. Useful for verifying DNS configuration after adding multiple domains.
--
-- ==== __Examples__
--
-- @
-- results <- checkDomainRecords client serviceId versionNum
-- mapM_ (\\(domain, cname, isValid) -> do
--   putStrLn $ domainName domain ++ ": " ++ if isValid then "OK" else "needs CNAME to " ++ cname
-- ) results
-- @
checkDomainRecords :: MonadFastly m =>
                   ServiceId
                   -> ServiceVersionNumber
                   -> m [(Domain, Text, Bool)]
checkDomainRecords (ServiceId sid) (ServiceVersionNumber v) = fastlyGet $ \r ->
  r { path = "/service/" <> encodeUtf8 sid <> "/version/" <> BS.pack (show v) <> "/domain/check_all" }
