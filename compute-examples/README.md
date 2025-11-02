# Fastly Compute@Edge Examples for Haskell

This directory contains examples and tooling for running Haskell applications on Fastly's Compute@Edge platform, compiled to WebAssembly.

## What is Fastly Compute?

Fastly Compute@Edge is a serverless computing platform that runs WebAssembly at Fastly's global edge network. It provides:

- ⚡ **Ultra-fast cold starts** (~1ms)
- 🌍 **Global distribution** at 300+ edge locations
- 🔒 **Secure sandbox** using WebAssembly
- 🚀 **High performance** close to your users
- 💻 **Language flexibility** via WASI

## Why Haskell + WebAssembly + Fastly?

Combining these technologies gives you:

1. **Type Safety at the Edge**: Haskell's powerful type system prevents bugs before deployment
2. **Functional Programming**: Pure functions, immutability, and composability for edge logic
3. **Performance**: GHC's optimizations + WebAssembly's near-native speed
4. **Portability**: WebAssembly runs anywhere, not tied to specific runtimes
5. **Global Reach**: Deploy once, run everywhere with Fastly's network

## Examples

### [hello-world](./hello-world)

A minimal "Hello, World!" application demonstrating:
- Basic Fastly Compute application structure
- FFI bindings to Fastly hostcalls
- Request/response handling
- Complete build and deployment workflow

**Start here** if you're new to Haskell on Compute.

### Future Examples (Coming Soon)

- **backend-requests**: Making HTTP requests to origin servers
- **routing**: URL-based routing and request transformation
- **kv-store**: Using Fastly's Key-Value store
- **json-api**: Building a JSON API at the edge
- **auth**: Authentication and authorization patterns
- **edge-rendering**: HTML rendering at the edge

## Quick Start

```bash
# 1. Set up GHC WebAssembly toolchain (see hello-world/README.md)

# 2. Build the example
cd hello-world
./build.sh

# 3. Test locally
viceroy bin/main.wasm

# 4. Deploy to Fastly (requires Fastly account)
fastly compute publish
```

## Using the Library in Your Own Project

Add to your `cabal.project`:

```cabal
packages:
  .

source-repository-package
  type: git
  location: https://github.com/iand675/fastly
  subdir: packages/fastly-compute
```

Or use it locally:

```cabal
packages:
  .
  path/to/fastly/packages/fastly-compute
```

Then in your `.cabal` file:

```cabal
build-depends:
  base,
  bytestring,
  fastly-compute
```

## Architecture Overview

```
┌──────────────────────────────────────────────┐
│         Haskell Application Code             │
│          (Your business logic)               │
└─────────────────┬────────────────────────────┘
                  │
                  │ imports
                  ▼
┌──────────────────────────────────────────────┐
│       Fastly.Compute High-Level API          │
│   (Request, Response, routing, etc.)         │
└─────────────────┬────────────────────────────┘
                  │
                  │ uses
                  ▼
┌──────────────────────────────────────────────┐
│       Fastly.Compute.FFI Bindings            │
│   (Foreign imports to Wasm hostcalls)        │
└─────────────────┬────────────────────────────┘
                  │
                  │ runtime linkage
                  ▼
┌──────────────────────────────────────────────┐
│     Fastly Compute Runtime (Wasmtime)        │
│   (Provides hostcalls, HTTP, KV, etc.)       │
└──────────────────────────────────────────────┘
```

## How It Works

1. **Compilation**:
   - Write Haskell code using `Fastly.Compute` API
   - Compile with `wasm32-wasi-ghc` to WebAssembly
   - Output is a `.wasm` file with embedded GHC runtime

2. **Deployment**:
   - Upload `.wasm` to Fastly Compute service
   - Fastly distributes to edge locations globally
   - No servers to manage, automatic scaling

3. **Execution**:
   - HTTP request arrives at edge location
   - Fastly instantiates your Wasm module (~1ms)
   - Your `main` function runs, returns response
   - Response sent back to client

## Components

### 1. The `fastly-compute` Library

Located at `../packages/fastly-compute/`, this is a proper Haskell package that provides:

**FFI Bindings** (`Fastly.Compute.FFI`):
- Low-level bindings to Fastly's WebAssembly hostcalls
- HTTP request/response handling
- Body read/write operations
- Header manipulation
- C header files for FFI declarations

**High-Level API** (`Fastly.Compute`):
- Type-safe Request and Response types
- Simple `runCompute` entry point
- Automatic error handling

```haskell
import Fastly.Compute

main :: IO ()
main = runCompute $ \req -> do
  return $ Response
    { responseStatus = 200
    , responseHeaders = [("Content-Type", "text/plain")]
    , responseBody = "Hello from Haskell!"
    }
```

### 2. Example Applications

The `hello-world/` directory contains a complete example that uses the `fastly-compute` library.

### 3. Build Tooling

- Multi-package `cabal.project` configuration
- Build scripts for WebAssembly compilation
- Development and deployment workflows

## Requirements

- **GHC 9.6+** with WebAssembly backend support
- **Cabal 3.8+**
- **Fastly CLI** (for deployment)
- **Viceroy** (for local testing)

See [hello-world/README.md](./hello-world/README.md) for detailed setup instructions.

## Toolchain Setup Summary

```bash
# 1. Install GHC Wasm backend
git clone https://gitlab.haskell.org/ghc/ghc-wasm-meta.git
cd ghc-wasm-meta
./setup.sh

# 2. Add to PATH
export PATH="$HOME/.ghc-wasm/wasm32-wasi-ghc-9.10.1/bin:$PATH"

# 3. Install Fastly tools
brew install fastly/tap/fastly
cargo install viceroy

# 4. You're ready!
cd hello-world && ./build.sh
```

## Performance Notes

- **Cold Start**: ~1ms (Fastly's Wasm instantiation is very fast)
- **Binary Size**: 5-15 MB typical (includes GHC runtime)
- **Memory Limit**: 128 MB per request
- **CPU Time Limit**: 50ms per request
- **Network**: Millisecond latency to backends

## Current Status

🟢 **Working**:
- Basic HTTP request/response handling
- FFI bindings to core hostcalls
- Local testing with Viceroy
- Deployment to Fastly Compute

🟡 **Partial**:
- Request parsing (basic implementation)
- Error handling (needs improvement)

🔴 **TODO**:
- Backend request support
- Request header/URL parsing
- KV Store bindings
- Config Store bindings
- Geolocation API
- Edge rate limiting
- Logging integration

## Development Workflow

### 1. Local Development

```bash
# Edit your Haskell code
vim src/Main.hs

# Build
./build.sh

# Test locally
viceroy bin/main.wasm

# Test with curl
curl http://localhost:7878
```

### 2. Deployment

```bash
# Deploy to Fastly
fastly compute publish

# View logs
fastly log-tail
```

### 3. Debugging

```bash
# Run with verbose output
viceroy -v bin/main.wasm

# Check Wasm module info
wasm-objdump -x bin/main.wasm

# Analyze size
wasm-opt --print-size bin/main.wasm
```

## Best Practices

1. **Keep handlers pure when possible**: Easier to test and reason about
2. **Minimize binary size**: Use `-O2`, consider code splitting for large apps
3. **Handle errors gracefully**: Return 5xx responses rather than crashing
4. **Cache aggressively**: Use surrogate keys for efficient cache invalidation
5. **Monitor performance**: Use Fastly's real-time analytics

## Comparison with Other Languages

| Feature | Rust | JavaScript | Go | **Haskell** |
|---------|------|------------|----|----|
| Type Safety | ✅ Strong | ⚠️ Weak | ✅ Strong | ✅ Very Strong |
| Binary Size | ✅ Small (100KB-1MB) | ✅ Small (500KB-2MB) | ✅ Small (1-3MB) | ⚠️ Large (5-15MB) |
| Cold Start | ✅ ~1ms | ✅ ~1ms | ✅ ~1ms | ✅ ~1ms |
| Maturity | ✅ Official SDK | ✅ Official SDK | ✅ Official SDK | ⚠️ Experimental |
| Learning Curve | ⚠️ Steep | ✅ Easy | ✅ Moderate | ⚠️ Steep |

**Haskell's sweet spot**: Complex business logic that benefits from strong typing and functional programming.

## Troubleshooting

See [hello-world/README.md#troubleshooting](./hello-world/README.md#troubleshooting) for common issues and solutions.

## Resources

- [Fastly Compute Documentation](https://developer.fastly.com/learning/compute/)
- [GHC WebAssembly Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)
- [ghc-wasm-meta Setup](https://gitlab.haskell.org/ghc/ghc-wasm-meta)
- [Fastly Compute ABI](https://github.com/fastly/compute-at-edge-abi)
- [Viceroy Testing Tool](https://github.com/fastly/Viceroy)

## Contributing

This is an experimental project and contributions are very welcome! Areas where help would be appreciated:

- Implementing additional hostcall bindings
- Creating more example applications
- Improving documentation
- Optimizing binary size
- Adding tests
- Performance benchmarking

## License

BSD-3-Clause - see LICENSE files in individual examples

## Acknowledgments

- Fastly for the Compute@Edge platform and excellent documentation
- GHC team for WebAssembly backend support
- The Haskell community
- All contributors

---

**Note**: This is an experimental project demonstrating Haskell on Fastly Compute. For production use, consider the official Rust, JavaScript, or Go SDKs which have more features and better support.
