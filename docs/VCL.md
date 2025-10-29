# Fastly VCL Module

The `Network.Fastly.VCL` module provides comprehensive support for parsing, rendering, and generating Fastly VCL (Varnish Configuration Language) code in Haskell.

## Features

- **Parsing**: Parse VCL code into a typed AST using megaparsec
- **Rendering**: Pretty-print VCL AST back to formatted text using prettyprinter
- **Generation**: Build VCL programmatically using a convenient builder API
- **Type-safe**: Strongly typed AST ensures correctness
- **Round-trip**: Parse and render VCL code while preserving semantics

## Installation

Add `fastly` to your project dependencies. The VCL module requires:
- `megaparsec >= 9.0`
- `prettyprinter >= 1.7`

```cabal
build-depends: fastly
```

## Quick Start

### Parsing VCL

```haskell
import Network.Fastly.VCL

let vclCode = "sub vcl_recv { set req.http.Host = \"example.com\"; }"
case parseVCL vclCode of
  Left err -> print err
  Right vcl -> putStrLn "Parsed successfully!"
```

### Generating VCL

```haskell
import Network.Fastly.VCL

let vcl = VCL
      [ subroutine VclRecv
        [ setVar ["req", "http", "Host"] (stringLit "example.com")
        , returnWith "pass"
        ]
      ]

putStrLn $ renderVCL vcl
```

## VCL AST Types

### Top-Level Declarations

- `VCL` - A complete VCL document
- `TopLevel` - Top-level declarations:
  - `TopLevelSubroutine` - Subroutine definition
  - `TopLevelACL` - ACL definition
  - `TopLevelBackend` - Backend definition
  - `TopLevelDirector` - Director (load balancer)
  - `TopLevelTable` - Table (edge dictionary)
  - `TopLevelInclude` - Include another file
  - `TopLevelImport` - Import a module

### Subroutines

Subroutines can be predefined (like `vcl_recv`, `vcl_deliver`) or custom:

```haskell
data SubroutineName
  = VclRecv        -- vcl_recv
  | VclHash        -- vcl_hash
  | VclHit         -- vcl_hit
  | VclMiss        -- vcl_miss
  | VclPass        -- vcl_pass
  | VclFetch       -- vcl_fetch
  | VclError       -- vcl_error
  | VclDeliver     -- vcl_deliver
  | VclLog         -- vcl_log
  | CustomSub Text -- User-defined
```

### Statements

VCL supports various statements:

- `Set` - Set a variable: `set req.http.Host = "example.com";`
- `Unset` - Unset a variable: `unset req.http.Cookie;`
- `Declare` - Declare a local variable: `declare local var.foo STRING;`
- `If` - Conditional: `if (condition) { ... } elsif { ... } else { ... }`
- `Return` - Return from subroutine: `return;` or `return(pass);`
- `Call` - Call another subroutine: `call my_sub;`
- `Log` - Log a message: `log "message";`
- `Error` - Generate error: `error 404 "Not Found";`
- `Restart` - Restart request: `restart;`
- `Synthetic` - Generate synthetic response: `synthetic "content";`

### Expressions

Expressions support:

- **Literals**: strings, integers, floats, booleans, durations
- **Variables**: dotted paths like `req.http.Host`
- **Binary operations**: arithmetic, comparison, logical, regex matching
- **Unary operations**: negation, logical NOT
- **Function calls**: `func(arg1, arg2)`

### Operators

- **Arithmetic**: `+`, `-`, `*`, `/`, `%`
- **Comparison**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- **Logical**: `&&`, `||`, `!`
- **Regex**: `~` (match), `!~` (not match)

### Data Types

- `STRING` - Text strings
- `INTEGER` - Integers
- `FLOAT` - Floating-point numbers
- `BOOL` - Booleans
- `TIME` - Absolute time
- `RTIME` - Relative time/duration
- `IP` - IP addresses
- `ACL` - Access control lists
- `BACKEND` - Backend servers

## Builder API

The module provides convenient builder functions for constructing VCL programmatically.

### Subroutine Builders

```haskell
subroutine :: SubroutineName -> [Statement] -> TopLevel
setVar :: [Text] -> Expr -> Statement
unsetVar :: [Text] -> Statement
ifStmt :: Expr -> [Statement] -> Statement
ifElse :: Expr -> [Statement] -> [Statement] -> Statement
returnStmt :: Statement
returnWith :: Text -> Statement
callSub :: SubroutineName -> Statement
logStmt :: Expr -> Statement
```

### Expression Builders

```haskell
stringLit :: Text -> Expr
intLit :: Int -> Expr
floatLit :: Double -> Expr
boolLit :: Bool -> Expr
durationLit :: Text -> Expr
var :: [Text] -> Expr

-- Operators (infix functions)
(.==.), (./=.), (.<.), (.<=.), (.>.), (.>=.) :: Expr -> Expr -> Expr
(.&&.), (.||.) :: Expr -> Expr -> Expr
(.~.), (.!~.) :: Expr -> Expr -> Expr
(.+.), (.-.), (.*.), (./.), (.%.) :: Expr -> Expr -> Expr

notExpr :: Expr -> Expr
negExpr :: Expr -> Expr
funcCall :: Text -> [Expr] -> Expr
```

## Examples

### Example 1: Basic Subroutine

```haskell
let vcl = VCL
      [ subroutine VclRecv
        [ ifStmt (var ["req", "http", "Host"] .==. stringLit "example.com")
          [ setVar ["req", "backend"] (var ["my_backend"])
          , returnWith "pass"
          ]
        ]
      ]

putStrLn $ renderVCL vcl
```

Output:
```vcl
sub vcl_recv {
  if (req.http.Host == "example.com") {
    set req.backend = my_backend;
    return(pass);
  }
}
```

### Example 2: Backend Definition

```haskell
let backend = TopLevelBackend $ Backend (Identifier "my_backend")
      [ BackendHost "origin.example.com"
      , BackendPort 443
      , BackendSSL True
      , BackendConnectTimeout "1s"
      , BackendFirstByteTimeout "15s"
      ]

let vcl = VCL [backend]
putStrLn $ renderVCL vcl
```

Output:
```vcl
backend my_backend {
  .host = "origin.example.com";
  .port = "443";
  .ssl = true;
  .connect_timeout = 1s;
  .first_byte_timeout = 15s;
}
```

### Example 3: ACL Definition

```haskell
let acl = TopLevelACL $ ACL (Identifier "allowed_ips")
      [ ACLEntry False "192.168.1.0/24"
      , ACLEntry False "10.0.0.0/8"
      , ACLEntry True "10.0.1.100"  -- Negated
      ]

let vcl = VCL [acl]
putStrLn $ renderVCL vcl
```

Output:
```vcl
acl allowed_ips {
  "192.168.1.0/24";
  "10.0.0.0/8";
  !"10.0.1.100";
}
```

### Example 4: Complex VCL with Multiple Features

```haskell
let vcl = VCL
      [ -- Backend
        TopLevelBackend $ Backend (Identifier "origin")
          [ BackendHost "example.com"
          , BackendPort 443
          ]

      , -- ACL
        TopLevelACL $ ACL (Identifier "trusted")
          [ ACLEntry False "192.168.0.0/16" ]

      , -- vcl_recv
        subroutine VclRecv
          [ setVar ["req", "http", "X-Forwarded-For"]
              (funcCall "client.ip" [])
          , ifStmt (var ["req", "http", "Host"] .==. stringLit "example.com")
              [ returnWith "lookup" ]
          , returnWith "pass"
          ]

      , -- vcl_deliver
        subroutine VclDeliver
          [ setVar ["resp", "http", "X-Served-By"] (stringLit "Fastly")
          , unsetVar ["resp", "http", "X-Internal"]
          ]
      ]

putStrLn $ renderVCL vcl
```

## Parsing

### Parse Functions

```haskell
parseVCL :: Text -> Either ParseError VCL
parseVCLFile :: FilePath -> Text -> Either ParseError VCL
parseExpr :: Text -> Either ParseError Expr
parseStatement :: Text -> Either ParseError Statement
```

### Error Handling

The parser returns detailed error messages on failure:

```haskell
case parseVCL vclCode of
  Left err -> do
    putStrLn "Parse error:"
    print err  -- Shows line, column, and error details
  Right vcl ->
    -- Process the parsed VCL
```

## Rendering

### Render Functions

```haskell
renderVCL :: VCL -> Text
renderVCLWith :: RenderConfig -> VCL -> Text

data RenderConfig = RenderConfig
  { renderWidth :: Int    -- Maximum line width (default: 80)
  , renderIndent :: Int   -- Spaces per indent (default: 2)
  }

defaultRenderConfig :: RenderConfig
```

### Pretty-printing

For display purposes, you can use the pretty-printing functions:

```haskell
prettyVCL :: VCL -> Doc ann
prettyTopLevel :: TopLevel -> Doc ann
prettySubroutine :: Subroutine -> Doc ann
prettyStatement :: Statement -> Doc ann
prettyExpr :: Expr -> Doc ann
```

## Testing

The module includes comprehensive tests covering:

- Expression parsing
- Statement parsing
- Subroutine parsing
- Backend and ACL parsing
- Pretty-printing
- Round-trip parsing and rendering
- Builder API

Run tests with:
```bash
cabal test
```

## Limitations and Known Issues

1. **VCL Version**: The parser supports core VCL syntax compatible with Fastly. Some advanced or version-specific features may not be fully supported.

2. **Comments**: Comments are currently parsed and discarded during parsing. They are not preserved in the AST.

3. **Whitespace**: While the pretty-printer produces nicely formatted code, it doesn't preserve the original whitespace from parsed code.

4. **Error Recovery**: The parser uses standard megaparsec error recovery, which stops at the first syntax error.

## Contributing

When contributing VCL-related code:

1. Add tests for new features
2. Update documentation
3. Ensure round-trip parsing works for new constructs
4. Follow the existing code style

## References

- [Fastly VCL Documentation](https://www.fastly.com/documentation/reference/vcl/)
- [Varnish VCL Syntax](https://varnish-cache.org/docs/trunk/reference/vcl.html)
- [megaparsec Documentation](https://hackage.haskell.org/package/megaparsec)
- [prettyprinter Documentation](https://hackage.haskell.org/package/prettyprinter)

## License

BSD3 - See LICENSE file for details.

## Author

Ian Duncan (ian@iankduncan.com)
