# fastly

Type-safe Haskell client library for the Fastly CDN API.

## Overview

This library provides a comprehensive, type-safe interface to the Fastly API, allowing you to programmatically manage your Fastly CDN services.

## Features

- ✅ Service management and version control
- ✅ Cache purging (URL, surrogate key, purge all)
- ✅ Edge dictionary management (with batch operations)
- ✅ Domain configuration and DNS verification
- ✅ Gzip compression settings
- ✅ Cache status checking
- ✅ Public IP list retrieval

## Installation

```bash
cabal install fastly
```

Or add to your `.cabal` file:

```cabal
build-depends:
  fastly
```

## Quick Start

```haskell
import Network.Fastly

main :: IO ()
main = do
  result <- fastly "your-api-token" $ \client -> do
    -- List all services
    services <- listServices client
    return services

  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right services -> do
      putStrLn $ "Found " ++ show (length services) ++ " services"
      mapM_ print services
```

## Usage Examples

### Purge Cached Content

```haskell
-- Purge a specific URL
result <- fastly token $ \client ->
  purge client Instant "https://example.com/page.html"

-- Purge by surrogate key
result <- fastly token $ \client ->
  purgeKey client (SurrogateKey "my-key") Soft

-- Purge everything
result <- fastly token $ \client ->
  purgeAll client (ServiceId "service-id")
```

### Manage Edge Dictionaries

```haskell
-- Create a dictionary item
result <- fastly token $ \client ->
  createDictionaryItem client serviceId version dictId "key" "value"

-- Batch update dictionary items
let ops = [ DictionaryItemCreate "key1" "value1"
          , DictionaryItemUpdate "key2" "value2"
          , DictionaryItemDelete "key3"
          ]
result <- fastly token $ \client ->
  batchEditDictionaryItems client serviceId dictId ops
```

### Service Management

```haskell
-- Get service details
result <- fastly token $ \client ->
  getServiceDetails client (ServiceId "your-service-id")

-- Create a new service version
result <- fastly token $ \client ->
  cloneServiceVersion client serviceId currentVersion
```

## API Coverage

This library covers:

- **Services**: CRUD operations, version management
- **Purging**: URL, surrogate key, and service-wide purges
- **Dictionaries**: Full dictionary and item management with batch operations
- **Domains**: Domain configuration and DNS verification
- **Gzip**: Compression configuration
- **Utilities**: Cache status, public IPs

## Documentation

See the [module documentation](./src/Network/Fastly.hs) for detailed API reference.

## Related Packages

- [fastly-compute](../fastly-compute/) - Build edge applications with Haskell + WebAssembly

## Contributing

Contributions welcome! Areas for improvement:

- Additional API endpoint coverage
- Better error messages
- More examples
- Improved test coverage

## License

BSD-3-Clause

## Resources

- [Fastly API Documentation](https://developer.fastly.com/reference/api/)
- [Fastly API Authentication](https://docs.fastly.com/en/guides/using-api-tokens)
