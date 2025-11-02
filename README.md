# Fastly Haskell Libraries

Comprehensive Haskell support for Fastly's CDN and edge computing platform.

## What's Inside

This repository contains two complementary Haskell libraries for Fastly:

### 1. Fastly API Client Library

A type-safe Haskell client for the Fastly CDN API, allowing you to programmatically manage your Fastly services.

**Location**: `src/Network/Fastly/`

**Use cases**:
- Manage services and configurations
- Purge cached content
- Configure edge dictionaries
- Manage domains
- Configure compression and other settings

**Example**:
```haskell
import Network.Fastly

main :: IO ()
main = do
  result <- fastly "your-api-token" $ \client -> do
    services <- listServices client
    return services

  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right services -> mapM_ print services
```

See the [API documentation](./src/Network/Fastly.hs) for more details.

### 2. Fastly Compute@Edge Support (NEW!)

Run Haskell applications at the edge using Fastly's Compute@Edge platform, compiled to WebAssembly.

**Location**: `compute-examples/`

**Use cases**:
- Edge request/response handling
- URL routing and transformation
- A/B testing and feature flags
- Authentication at the edge
- Custom caching logic
- API composition

**Example**:
```haskell
import Fastly.Compute

main :: IO ()
main = runCompute $ \req -> do
  return $ Response
    { responseStatus = 200
    , responseHeaders = [("Content-Type", "text/plain")]
    , responseBody = "Hello from Haskell at the edge!"
    }
```

See the [Compute@Edge README](./compute-examples/README.md) for setup and examples.

## Project Structure

This is a multi-package repository:

```
fastly/
├── packages/
│   ├── fastly/              # API client library
│   └── fastly-compute/      # Compute@Edge library
├── compute-examples/
│   └── hello-world/         # Example Compute application
├── cabal.project            # Multi-package configuration
└── stack.yaml               # Stack multi-package configuration
```

## Quick Start

### Option 1: With Nix (Easiest) 🎯

```bash
# Enter development environment
nix develop

# Build API client
cabal build fastly

# Build Compute example
cd compute-examples/hello-world
./build.sh
viceroy bin/main.wasm
```

See [Nix Setup Guide](./nix/README.md) for more options.

### Option 2: Manual Setup

#### For API Management (Control Plane)

```bash
# Using stack
stack build fastly
stack ghci fastly

# Using cabal
cabal build fastly
cabal repl fastly
```

#### For Edge Computing (Data Plane)

```bash
cd compute-examples/hello-world

# Build the WebAssembly module
./build.sh

# Test locally
viceroy bin/main.wasm

# Deploy to Fastly
fastly compute publish
```

## Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                    Fastly Platform                       │
├────────────────────────┬────────────────────────────────┤
│   Control Plane        │      Data Plane                 │
│   (API Management)     │   (Edge Computing)              │
├────────────────────────┼────────────────────────────────┤
│ • Manage services      │ • Handle requests               │
│ • Configure CDN        │ • Transform data                │
│ • Purge cache          │ • Route traffic                 │
│ • Update dictionaries  │ • Execute logic at edge         │
├────────────────────────┼────────────────────────────────┤
│ Network.Fastly         │ Fastly.Compute                  │
│ (This library)         │ (WebAssembly)                   │
│                        │                                 │
│ Standard HTTP client   │ WASI + Fastly hostcalls        │
│ Runs anywhere          │ Runs at Fastly edge             │
└────────────────────────┴────────────────────────────────┘
```

## Features

### API Client Library

- ✅ Services and versions
- ✅ Purging (URL, surrogate key, purge all)
- ✅ Edge dictionaries (with batch operations)
- ✅ Domains (with DNS verification)
- ✅ Gzip configuration
- ✅ Cache status checking
- ✅ Public IP list

### Compute@Edge (Experimental)

- ✅ Basic HTTP request/response handling
- ✅ FFI bindings to Fastly hostcalls
- ✅ High-level Haskell API
- ✅ Local testing with Viceroy
- ✅ WebAssembly compilation via GHC
- 🚧 Backend requests (TODO)
- 🚧 KV Store (TODO)
- 🚧 Full request parsing (TODO)

## Requirements

### Quick Start with Nix (Recommended) 🎯

The easiest way to get started is with Nix, which provides all tools automatically:

```bash
# Using Flakes (recommended)
nix develop

# Or with direnv (auto-loads when you cd here)
direnv allow

# Traditional Nix
nix-shell
```

See [Nix Setup Guide](./nix/README.md) for details.

### Manual Setup

#### API Client

- GHC 9.10+ (tested with 9.10.1)
- Cabal 3.0+ or Stack
- Dependencies: aeson, http-client, bytestring, etc.

#### Compute@Edge

- GHC 9.6+ with WebAssembly backend
- wasm32-wasi toolchain
- Fastly CLI
- Viceroy (for local testing)

See [Compute@Edge setup guide](./compute-examples/hello-world/README.md) for detailed manual setup.

## Installation

### API Client

Add to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - fastly
```

Or install directly:

```bash
cabal install fastly
```

### Compute@Edge Library

Add to your `cabal.project`:

```cabal
packages:
  .
  path/to/fastly-compute
```

Or use as a git dependency:

```cabal
source-repository-package
  type: git
  location: https://github.com/iand675/fastly
  tag: <version>
  subdir: packages/fastly-compute
```

## Documentation

- [API Client Documentation](./packages/fastly/src/Network/Fastly.hs) - Control plane API reference
- [API Client Package](./packages/fastly/) - Full API client package
- [Compute@Edge Library](./packages/fastly-compute/) - Compute library package
- [Compute@Edge Guide](./compute-examples/README.md) - Data plane development guide
- [Hello World Example](./compute-examples/hello-world/README.md) - Getting started with Compute

## Examples

### API Client Examples

```haskell
-- Purge a URL
result <- fastly apiToken $ \client ->
  purge client Instant "https://example.com/page.html"

-- Update edge dictionary
result <- fastly apiToken $ \client ->
  upsertDictionaryItem client serviceId version dictId "key" "value"

-- List all services
result <- fastly apiToken $ \client ->
  listServices client
```

### Compute@Edge Examples

See the [compute-examples/](./compute-examples/) directory for:
- Hello World (basic request/response)
- More examples coming soon!

## Use Cases

### When to use the API Client

- Automating CDN configuration
- Building deployment tools
- Creating custom dashboards
- Integrating Fastly into CI/CD
- Managing multiple services programmatically

### When to use Compute@Edge

- Custom request routing
- Edge authentication/authorization
- Request/response transformation
- A/B testing at the edge
- Geolocation-based logic
- API composition and aggregation
- Custom caching strategies

## Performance

### API Client

- Standard HTTP client library
- Respects Fastly API rate limits
- Suitable for management operations

### Compute@Edge

- Cold start: ~1ms
- Memory limit: 128 MB per request
- CPU time limit: 50ms per request
- Binary size: 5-15 MB (with GHC runtime)
- Global deployment at 300+ edge locations

## Development Status

| Component | Status | Notes |
|-----------|--------|-------|
| API Client | ✅ Stable | Production-ready for GHC 9.10+ |
| Compute FFI Bindings | ⚠️ Experimental | Core functionality working |
| Compute High-Level API | ⚠️ Experimental | Basic features implemented |
| Backend Requests | 🔴 TODO | Not yet implemented |
| KV Store | 🔴 TODO | Not yet implemented |

## Contributing

Contributions welcome! This project needs help with:

- API client: Adding support for more Fastly API endpoints
- Compute: Implementing additional hostcall bindings
- Examples: More Compute@Edge example applications
- Documentation: Improving guides and API docs
- Testing: Adding more test coverage
- Performance: Optimizing WebAssembly binary size

## Testing

### API Client

```bash
cabal test
```

### Compute@Edge

```bash
cd compute-examples/hello-world
./build.sh
viceroy bin/main.wasm
curl http://localhost:7878
```

## Troubleshooting

### API Client

- Ensure you have a valid Fastly API token
- Check rate limit headers in error responses
- Verify service IDs and version numbers

### Compute@Edge

- Make sure `wasm32-wasi-ghc` is in your PATH
- Check that all C header files are present
- Test locally with Viceroy before deploying
- See the [troubleshooting guide](./compute-examples/hello-world/README.md#troubleshooting)

## Resources

### General
- [Fastly Documentation](https://docs.fastly.com/)
- [Fastly API Reference](https://developer.fastly.com/reference/api/)

### API Client
- [Fastly API](https://developer.fastly.com/reference/api/)
- [API Authentication](https://docs.fastly.com/en/guides/using-api-tokens)

### Compute@Edge
- [Compute Documentation](https://developer.fastly.com/learning/compute/)
- [GHC WebAssembly Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)
- [Compute ABI Spec](https://github.com/fastly/compute-at-edge-abi)
- [Viceroy](https://github.com/fastly/Viceroy)

## License

BSD-3-Clause License

Copyright (c) 2018-2025 Ian Duncan

See [LICENSE](./LICENSE) file for details.

## Changelog

### 0.2.0.0 (2025)
- Added Compute@Edge support with WebAssembly
- FFI bindings to Fastly hostcalls
- Hello World example application
- Comprehensive documentation and build tooling
- Upgraded to GHC 9.10 support

### 0.1.0.0
- Initial release
- Core API client functionality
- Services, purging, dictionaries, domains, gzip

## Acknowledgments

- Fastly for the excellent platform and documentation
- GHC team for WebAssembly backend support
- The Haskell community
- All contributors

## Related Projects

- [fastly-rust](https://github.com/fastly/fastly-rust) - Official Rust SDK
- [compute-sdk-go](https://github.com/fastly/compute-sdk-go) - Official Go SDK
- [js-compute](https://github.com/fastly/js-compute-runtime) - Official JavaScript SDK

---

**Status**: The API client is production-ready for GHC 9.10+. The Compute@Edge support is experimental but functional for basic use cases.

For questions, issues, or contributions, please open an issue on GitHub!
