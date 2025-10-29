# Fastly VCL Module

The `Network.Fastly.VCL` module provides comprehensive support for parsing, rendering, and generating Fastly VCL (Varnish Configuration Language) code in Haskell.

## Features

- **Parsing**: Parse VCL code into a typed AST using megaparsec
- **Rendering**: Pretty-print VCL AST back to formatted text using prettyprinter
- **Generation**: Build VCL programmatically using a convenient builder API
- **Validation**: Semantic validation to catch errors beyond syntax
- **Type-safe**: Strongly typed AST ensures correctness
- **Round-trip**: Parse and render VCL code while preserving semantics

## Installation

Add `fastly` to your project dependencies. The VCL module requires:
- `megaparsec >= 9.0`
- `prettyprinter >= 1.7`
- `containers >= 0.6`

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

### Validating VCL

```haskell
import Network.Fastly.VCL

let vclCode = "sub vcl_recv { set req.http.Host = \"example.com\"; return(pass); }"
case parseVCL vclCode of
  Left err -> print err
  Right vcl -> case validateVCL vcl of
    Left errors -> do
      putStrLn "Validation errors:"
      mapM_ print errors
    Right _ -> putStrLn "VCL is valid!"
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

## Validation

The validation system performs semantic checks on VCL code to catch errors that are syntactically valid but semantically incorrect. This includes type checking, variable scope validation, reference validation, and control flow analysis.

### Validation Functions

```haskell
validateVCL :: VCL -> ValidationResult VCL
validateTopLevel :: TopLevel -> ValidationResult TopLevel
validateSubroutine :: SubroutineContext -> Subroutine -> ValidationResult Subroutine
validateStatement :: SubroutineContext -> Statement -> ValidationResult Statement
validateExpr :: Expr -> ValidationResult Expr

type ValidationResult a = Either [ValidationError] a
```

### Validation Errors

The validation system can detect various types of errors:

#### Variable Errors

- `UndefinedVariable` - Variable used but not declared
- `InvalidVariableContext` - Variable used in wrong subroutine (e.g., `req.*` in `vcl_deliver`)
- `ReadOnlyVariable` - Attempted to modify read-only variable (e.g., `client.ip`)

#### Type Errors

- `TypeMismatch` - Type incompatibility in operations or assignments

#### Reference Errors

- `UndefinedSubroutine` - Called subroutine doesn't exist
- `UndefinedBackend` - Referenced backend doesn't exist
- `UndefinedACL` - Referenced ACL doesn't exist
- `DuplicateDefinition` - Duplicate backend, ACL, or subroutine name

#### Control Flow Errors

- `InvalidReturnAction` - Invalid return action for subroutine context
- `UnreachableCode` - Code after return/error/restart
- `MissingReturn` - Predefined subroutine missing return statement

### Subroutine Contexts

Different VCL subroutines have different validation rules:

```haskell
data SubroutineContext
  = RecvContext    -- vcl_recv - can use req.*, valid returns: lookup, pass, pipe, error, synth
  | HashContext    -- vcl_hash - can use req.*, valid returns: lookup, hash
  | HitContext     -- vcl_hit - can use obj.*, valid returns: deliver, pass, restart, synth
  | MissContext    -- vcl_miss - valid returns: fetch, pass, synth, error
  | PassContext    -- vcl_pass - valid returns: fetch, synth, error
  | FetchContext   -- vcl_fetch - can use bereq.*, beresp.*, valid returns: deliver, restart
  | ErrorContext   -- vcl_error - can use bereq.*, resp.*, valid returns: deliver, restart
  | DeliverContext -- vcl_deliver - can use resp.*, obj.*, valid returns: deliver, restart
  | LogContext     -- vcl_log - valid returns: deliver
  | CustomContext  -- custom subroutine - lenient validation
```

### Validation Examples

#### Example 1: Valid VCL

```haskell
let vclCode = unlines
      [ "backend origin {"
      , "  .host = \"example.com\";"
      , "}"
      , "sub vcl_recv {"
      , "  set req.http.Host = \"example.com\";"
      , "  return(pass);"
      , "}"
      ]

case parseVCL vclCode >>= validateVCL of
  Left errors -> mapM_ print errors
  Right vcl -> putStrLn "Valid VCL!"
```

#### Example 2: Invalid Variable Context

```haskell
let vclCode = unlines
      [ "sub vcl_deliver {"
      , "  set req.http.Host = \"example.com\";"  -- ERROR: req.* not available in vcl_deliver
      , "}"
      ]

case parseVCL vclCode >>= validateVCL of
  Left errors -> print errors
  -- Output: [InvalidVariableContext (Variable ["req","http","Host"]) DeliverContext]
  Right _ -> putStrLn "Valid"
```

#### Example 3: Type Mismatch

```haskell
let vclCode = unlines
      [ "sub vcl_recv {"
      , "  if (42) {"  -- ERROR: if requires boolean condition
      , "    return(pass);"
      , "  }"
      , "}"
      ]

case parseVCL vclCode >>= validateVCL of
  Left errors -> print errors
  -- Output: [TypeMismatch TBool TInteger "if condition"]
  Right _ -> putStrLn "Valid"
```

#### Example 4: Invalid Return Action

```haskell
let vclCode = unlines
      [ "sub vcl_recv {"
      , "  return(deliver);"  -- ERROR: deliver not valid in vcl_recv
      , "}"
      ]

case parseVCL vclCode >>= validateVCL of
  Left errors -> print errors
  -- Output: [InvalidReturnAction (Identifier "deliver") RecvContext]
  Right _ -> putStrLn "Valid"
```

#### Example 5: Undefined Reference

```haskell
let vclCode = unlines
      [ "sub vcl_recv {"
      , "  call undefined_subroutine;"  -- ERROR: subroutine not defined
      , "}"
      ]

case parseVCL vclCode >>= validateVCL of
  Left errors -> print errors
  -- Output: [UndefinedSubroutine (CustomSub "undefined_subroutine")]
  Right _ -> putStrLn "Valid"
```

#### Example 6: Unreachable Code

```haskell
let vclCode = unlines
      [ "sub vcl_recv {"
      , "  return(pass);"
      , "  set req.http.Host = \"example.com\";"  -- ERROR: unreachable
      , "}"
      ]

case parseVCL vclCode >>= validateVCL of
  Left errors -> print errors
  -- Output: [UnreachableCode]
  Right _ -> putStrLn "Valid"
```

### Validation Rules

#### Variable Scope Rules

| Variable Prefix | Readable In | Writable In | Notes |
|----------------|-------------|-------------|-------|
| `req.*` | vcl_recv, vcl_hash | vcl_recv, vcl_hash | Client request |
| `bereq.*` | vcl_fetch, vcl_error | vcl_fetch, vcl_error | Backend request |
| `beresp.*` | vcl_fetch | vcl_fetch | Backend response |
| `resp.*` | vcl_deliver, vcl_error | vcl_deliver, vcl_error | Client response |
| `obj.*` | vcl_hit, vcl_deliver | - | Cached object (read-only) |
| `client.*` | All | - | Client info (read-only) |
| `server.*` | All | - | Server info (read-only) |
| `var.*` | All (if declared) | All (if declared) | Local variables |

#### Return Action Rules

| Subroutine | Valid Actions |
|------------|---------------|
| vcl_recv | lookup, pass, pipe, error, synth, hash |
| vcl_hash | lookup, hash |
| vcl_hit | deliver, pass, restart, synth, error |
| vcl_miss | fetch, pass, synth, error |
| vcl_pass | fetch, synth, error |
| vcl_fetch | deliver, deliver_stale, restart, error |
| vcl_error | deliver, restart |
| vcl_deliver | deliver, restart |
| vcl_log | deliver |

#### Type Compatibility

- Arithmetic operations (+, -, *, /, %) require INTEGER or FLOAT
- Comparison operations (==, !=, <, >, <=, >=) require same types
- Logical operations (&&, ||, !) require BOOL
- Regex operations (~, !~) require STRING
- String concatenation accepts any type (auto-converts)

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
- Validation (variable scope, type checking, references, control flow)

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
