{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Fastly.VCL.Parser
Description : Parser for Fastly VCL (Varnish Configuration Language)
Copyright   : (c) 2025 Ian Duncan
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental

This module provides a parser for VCL code using megaparsec.
-}

module Network.Fastly.VCL.Parser
  ( -- * Parsing functions
    parseVCL
  , parseVCLFile
  , parseExpr
  , parseStatement

    -- * Parser types
  , Parser
  , ParseError
  ) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Data.Char (isAlphaNum, isDigit)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Network.Fastly.VCL.Types

-- ---------------------------------------------------------------------------
-- Parser Types
-- ---------------------------------------------------------------------------

-- | The parser type for VCL.
type Parser = Parsec Void Text

-- | Parse errors.
type ParseError = ParseErrorBundle Text Void

-- ---------------------------------------------------------------------------
-- Lexer
-- ---------------------------------------------------------------------------

-- | Skip whitespace and comments.
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- | Parse a lexeme (token followed by whitespace).
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a specific symbol/keyword.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse a keyword (reserved word).
keyword :: Text -> Parser ()
keyword w = lexeme (string w *> notFollowedBy alphaNumChar)

-- | Parse something between braces.
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parse something between parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse something between double quotes.
quotes :: Parser a -> Parser a
quotes = between (char '"') (char '"')

-- | Parse a semicolon.
semi :: Parser ()
semi = void $ symbol ";"

-- | Parse a dot.
dot :: Parser ()
dot = void $ symbol "."

-- | Parse a comma.
comma :: Parser ()
comma = void $ symbol ","

-- | Reserved keywords in VCL.
reservedWords :: [Text]
reservedWords =
  [ "sub", "if", "else", "elsif", "elseif"
  , "set", "unset", "declare", "local"
  , "return", "call", "log", "error", "restart"
  , "synthetic", "include", "import"
  , "acl", "backend", "director", "table"
  , "true", "false"
  , "STRING", "INTEGER", "FLOAT", "BOOL", "TIME", "RTIME", "IP", "ACL", "BACKEND"
  ]

-- ---------------------------------------------------------------------------
-- Top-level Parsing
-- ---------------------------------------------------------------------------

-- | Parse a complete VCL document.
parseVCL :: Text -> Either ParseError VCL
parseVCL = parse (sc *> vclParser <* eof) "<input>"

-- | Parse a VCL file.
parseVCLFile :: FilePath -> Text -> Either ParseError VCL
parseVCLFile = parse (sc *> vclParser <* eof)

-- | VCL document parser.
vclParser :: Parser VCL
vclParser = VCL <$> many topLevelParser

-- | Top-level declaration parser.
topLevelParser :: Parser TopLevel
topLevelParser = choice
  [ TopLevelSubroutine <$> subroutineParser
  , TopLevelACL <$> aclParser
  , TopLevelBackend <$> backendParser
  , TopLevelDirector <$> directorParser
  , TopLevelTable <$> tableParser
  , TopLevelInclude <$> includeParser
  , TopLevelImport <$> importParser
  ]

-- ---------------------------------------------------------------------------
-- Subroutine Parsing
-- ---------------------------------------------------------------------------

-- | Parse a subroutine definition.
subroutineParser :: Parser Subroutine
subroutineParser = do
  keyword "sub"
  name <- subroutineNameParser
  body <- braces (many statementParser)
  return $ Subroutine name body

-- | Parse a subroutine name.
subroutineNameParser :: Parser SubroutineName
subroutineNameParser = lexeme $ choice
  [ VclRecv <$ string "vcl_recv"
  , VclHash <$ string "vcl_hash"
  , VclHit <$ string "vcl_hit"
  , VclMiss <$ string "vcl_miss"
  , VclPass <$ string "vcl_pass"
  , VclFetch <$ string "vcl_fetch"
  , VclError <$ string "vcl_error"
  , VclDeliver <$ string "vcl_deliver"
  , VclLog <$ string "vcl_log"
  , CustomSub <$> identifierText
  ]

-- ---------------------------------------------------------------------------
-- Statement Parsing
-- ---------------------------------------------------------------------------

-- | Parse a VCL statement.
statementParser :: Parser Statement
statementParser = choice
  [ setParser
  , unsetParser
  , declareParser
  , ifParser
  , returnParser
  , callParser
  , logParser
  , addHeaderParser
  , removeHeaderParser
  , errorParser
  , restartParser
  , syntheticParser
  , syntheticBase64Parser
  ]

-- | Parse a set statement.
setParser :: Parser Statement
setParser = do
  keyword "set"
  var <- variableParser
  symbol "="
  expr <- exprParser
  semi
  return $ Set var expr

-- | Parse an unset statement.
unsetParser :: Parser Statement
unsetParser = do
  keyword "unset"
  var <- variableParser
  semi
  return $ Unset var

-- | Parse a declare statement.
declareParser :: Parser Statement
declareParser = do
  keyword "declare"
  keyword "local"
  -- Parse "var.foo" and extract just "foo"
  _ <- string "var"
  _ <- char '.'
  identName <- lexeme identifierTextNoCheck
  typ <- vclTypeParser
  init <- optional (symbol "=" *> exprParser)
  semi
  return $ Declare (Identifier identName) typ init

-- | Parse an if statement.
ifParser :: Parser Statement
ifParser = do
  keyword "if"
  cond <- parens exprParser
  thenStmts <- braces (many statementParser)
  elsifs <- many elsifParser
  elseStmts <- optional (keyword "else" *> braces (many statementParser))
  return $ If cond thenStmts elsifs elseStmts

-- | Parse an elsif clause.
elsifParser :: Parser (Expr, [Statement])
elsifParser = do
  choice [keyword "elsif", keyword "elseif"]
  cond <- parens exprParser
  stmts <- braces (many statementParser)
  return (cond, stmts)

-- | Parse a return statement.
returnParser :: Parser Statement
returnParser = do
  keyword "return"
  action <- optional $ parens identifierParser
  semi
  return $ Return action

-- | Parse a call statement.
callParser :: Parser Statement
callParser = do
  keyword "call"
  name <- subroutineNameParser
  semi
  return $ Call name

-- | Parse a log statement.
logParser :: Parser Statement
logParser = do
  keyword "log"
  expr <- exprParser
  semi
  return $ Log expr

-- | Parse an add statement.
addHeaderParser :: Parser Statement
addHeaderParser = do
  keyword "add"
  ident <- identifierParser
  symbol "="
  expr <- exprParser
  semi
  return $ AddHeader ident expr

-- | Parse a remove statement.
removeHeaderParser :: Parser Statement
removeHeaderParser = do
  keyword "remove"
  ident <- identifierParser
  semi
  return $ RemoveHeader ident

-- | Parse an error statement.
errorParser :: Parser Statement
errorParser = do
  keyword "error"
  code <- lexeme L.decimal
  msg <- optional stringLiteralParser
  semi
  return $ Error code msg

-- | Parse a restart statement.
restartParser :: Parser Statement
restartParser = keyword "restart" *> semi *> return Restart

-- | Parse a synthetic statement.
syntheticParser :: Parser Statement
syntheticParser = do
  keyword "synthetic"
  expr <- exprParser
  semi
  return $ Synthetic expr

-- | Parse a synthetic.base64 statement.
syntheticBase64Parser :: Parser Statement
syntheticBase64Parser = do
  keyword "synthetic"
  dot
  keyword "base64"
  expr <- exprParser
  semi
  return $ SyntheticBase64 expr

-- ---------------------------------------------------------------------------
-- Expression Parsing
-- ---------------------------------------------------------------------------

-- | Parse a VCL expression (public API).
parseExpr :: Text -> Either ParseError Expr
parseExpr = parse (sc *> exprParser <* eof) "<expr>"

-- | Parse a VCL statement (public API).
parseStatement :: Text -> Either ParseError Statement
parseStatement = parse (sc *> statementParser <* eof) "<statement>"

-- | Parse an expression with operator precedence.
exprParser :: Parser Expr
exprParser = makeExprParser termParser operatorTable

-- | Operator precedence table.
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" (UnOp Neg)
    , prefix "!" (UnOp Not)
    ]
  , [ binary "*" (BinOp Mul)
    , binary "/" (BinOp Div)
    , binary "%" (BinOp Mod)
    ]
  , [ binary "+" (BinOp Add)
    , binary "-" (BinOp Sub)
    ]
  , [ binary "==" (BinOp Eq)
    , binary "!=" (BinOp Ne)
    , binary "<=" (BinOp Le)
    , binary ">=" (BinOp Ge)
    , binary "<" (BinOp Lt)
    , binary ">" (BinOp Gt)
    , binary "~" (BinOp Match)
    , binary "!~" (BinOp NotMatch)
    ]
  , [ binary "&&" (BinOp And) ]
  , [ binary "||" (BinOp Or) ]
  ]

-- | Create a binary operator parser.
binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

-- | Create a prefix operator parser.
prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

-- | Parse a term (atomic expression).
termParser :: Parser Expr
termParser = choice
  [ parens exprParser
  , Lit <$> literalParser
  , try functionCallParser
  , Var <$> variableParser
  ]

-- | Parse a function call.
functionCallParser :: Parser Expr
functionCallParser = do
  name <- identifierParser
  args <- parens (exprParser `sepBy` comma)
  return $ FunctionCall name args

-- | Parse a literal value.
literalParser :: Parser Literal
literalParser = choice
  [ LString <$> stringLiteralParser
  , LBool True <$ keyword "true"
  , LBool False <$ keyword "false"
  , try floatLiteralParser
  , try durationLiteralParser
  , LInteger <$> integerLiteralParser
  ]

-- | Parse a string literal.
stringLiteralParser :: Parser Text
stringLiteralParser = lexeme $ T.pack <$> quotes (many stringChar)
  where
    stringChar = choice
      [ char '\\' *> escapeChar
      , noneOf ['"', '\\']
      ]
    escapeChar = choice
      [ '"' <$ char '"'
      , '\\' <$ char '\\'
      , 'n' <$ char 'n'
      , 't' <$ char 't'
      , 'r' <$ char 'r'
      ]

-- | Parse an integer literal.
integerLiteralParser :: Parser Int64
integerLiteralParser = lexeme L.decimal

-- | Parse a float literal.
floatLiteralParser :: Parser Literal
floatLiteralParser = lexeme $ do
  f <- L.float
  return $ LFloat f

-- | Parse a duration literal (e.g., 10s, 5m, 1h).
durationLiteralParser :: Parser Literal
durationLiteralParser = lexeme $ do
  num <- some digitChar
  unit <- oneOf ['s', 'm', 'h', 'd', 'y']
  return $ LDuration $ T.pack (num ++ [unit])

-- ---------------------------------------------------------------------------
-- Type Parsing
-- ---------------------------------------------------------------------------

-- | Parse a VCL type.
vclTypeParser :: Parser VCLType
vclTypeParser = lexeme $ choice
  [ TString <$ keyword "STRING"
  , TInteger <$ keyword "INTEGER"
  , TFloat <$ keyword "FLOAT"
  , TBool <$ keyword "BOOL"
  , TTime <$ keyword "TIME"
  , TRTime <$ keyword "RTIME"
  , TIP <$ keyword "IP"
  , TACL <$ keyword "ACL"
  , TBackend <$ keyword "BACKEND"
  ]

-- ---------------------------------------------------------------------------
-- Identifier and Variable Parsing
-- ---------------------------------------------------------------------------

-- | Parse an identifier.
identifierParser :: Parser Identifier
identifierParser = Identifier <$> identifierText

-- | Parse identifier text.
identifierText :: Parser Text
identifierText = lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let ident = T.pack (first : rest)
  if ident `elem` reservedWords
    then fail $ "Reserved word: " ++ T.unpack ident
    else return ident

-- | Parse identifier text without reserved word check (for use in variables).
identifierTextNoCheck :: Parser Text
identifierTextNoCheck = do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_' <|> char '-')
  return $ T.pack (first : rest)

-- | Parse a variable (possibly dotted).
variableParser :: Parser Variable
variableParser = lexeme $ do
  parts <- identifierTextNoCheck `sepBy1` char '.'
  return $ Variable parts

-- ---------------------------------------------------------------------------
-- ACL Parsing
-- ---------------------------------------------------------------------------

-- | Parse an ACL definition.
aclParser :: Parser ACL
aclParser = do
  keyword "acl"
  name <- identifierParser
  entries <- braces (many aclEntryParser)
  return $ ACL name entries

-- | Parse an ACL entry.
aclEntryParser :: Parser ACLEntry
aclEntryParser = do
  negated <- (True <$ symbol "!") <|> return False
  ip <- lexeme $ quotes (T.pack <$> some (alphaNumChar <|> oneOf [':', '.', '/']))
  semi
  return $ ACLEntry negated ip

-- ---------------------------------------------------------------------------
-- Backend Parsing
-- ---------------------------------------------------------------------------

-- | Parse a backend definition.
backendParser :: Parser Backend
backendParser = do
  keyword "backend"
  name <- identifierParser
  props <- braces (many backendPropertyParser)
  return $ Backend name props

-- | Parse a backend property.
backendPropertyParser :: Parser BackendProperty
backendPropertyParser = do
  dot
  choice
    [ keyword "host" *> symbol "=" *> (BackendHost <$> stringLiteralParser) <* semi
    , keyword "port" *> symbol "=" *> (BackendPort <$> integerLiteralParser) <* semi
    , keyword "connect_timeout" *> symbol "=" *> (BackendConnectTimeout <$> durationText) <* semi
    , keyword "first_byte_timeout" *> symbol "=" *> (BackendFirstByteTimeout <$> durationText) <* semi
    , keyword "between_bytes_timeout" *> symbol "=" *> (BackendBetweenBytesTimeout <$> durationText) <* semi
    , keyword "ssl" *> symbol "=" *> (BackendSSL <$> boolParser) <* semi
    , keyword "ssl_cert_hostname" *> symbol "=" *> (BackendSSLCertHostname <$> stringLiteralParser) <* semi
    , keyword "ssl_sni_hostname" *> symbol "=" *> (BackendSSLSNIHostname <$> stringLiteralParser) <* semi
    , keyword "max_connections" *> symbol "=" *> (BackendMaxConnections <$> integerLiteralParser) <* semi
    , keyword "probe" *> symbol "=" *> (BackendProbe <$> stringLiteralParser) <* semi
    ]
  where
    durationText = durationLiteralParser >>= \(LDuration t) -> return t
    boolParser = (True <$ keyword "true") <|> (False <$ keyword "false")

-- ---------------------------------------------------------------------------
-- Director Parsing
-- ---------------------------------------------------------------------------

-- | Parse a director definition.
directorParser :: Parser Director
directorParser = do
  keyword "director"
  name <- identifierParser
  dirType <- directorTypeParser
  backends <- braces (many (dot *> symbol "backend" *> symbol "=" *> identifierParser <* semi))
  return $ Director name dirType backends

-- | Parse a director type.
directorTypeParser :: Parser DirectorType
directorTypeParser = lexeme $ choice
  [ Random <$ keyword "random"
  , RoundRobin <$ keyword "round-robin"
  , Hash <$ keyword "hash"
  , Client <$ keyword "client"
  ]

-- ---------------------------------------------------------------------------
-- Table Parsing
-- ---------------------------------------------------------------------------

-- | Parse a table definition.
tableParser :: Parser Table
tableParser = do
  keyword "table"
  name <- identifierParser
  props <- braces (many tablePropertyParser)
  return $ Table name props

-- | Parse a table property.
tablePropertyParser :: Parser TableProperty
tablePropertyParser = choice
  [ keyword "type" *> symbol "=" *> (TableType <$> vclTypeParser) <* semi
  , keyword "default" *> symbol "=" *> (TableDefault <$> literalParser) <* semi
  ]

-- ---------------------------------------------------------------------------
-- Include and Import Parsing
-- ---------------------------------------------------------------------------

-- | Parse an include statement.
includeParser :: Parser Text
includeParser = do
  keyword "include"
  path <- stringLiteralParser
  semi
  return path

-- | Parse an import statement.
importParser :: Parser Text
importParser = do
  keyword "import"
  mod <- identifierText
  semi
  return mod
