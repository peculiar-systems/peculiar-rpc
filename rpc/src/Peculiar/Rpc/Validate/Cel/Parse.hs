module Peculiar.Rpc.Validate.Cel.Parse
  ( parseExpr
  ) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Char (chr, isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Data.Word (Word64, Word8)
import Peculiar.Rpc.Validate.Arithmetic (digitValue)
import Peculiar.Rpc.Validate.Cel.Syntax hiding (Operator)
import Peculiar.Rpc.Validate.Cel.Value (CompileError (..), Value (..))
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char (char, char', space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void T.Text

parseExpr :: T.Text -> Either CompileError Expr
parseExpr = first (Unparsable . T.pack . errorBundlePretty) . parse (spaces *> expression <* eof) "expression"

spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaces

operator :: T.Text -> Parser ()
operator name = lexeme (void (try (string name <* notFollowedBy (satisfy (`T.elem` followers)))))
 where
  followers :: T.Text
  followers = case name of
    "<" -> "="
    ">" -> "="
    "!" -> "="
    "=" -> "="
    "|" -> "|"
    "&" -> "&"
    _ -> ""

keyword :: T.Text -> Parser ()
keyword name = lexeme (void (try (string name <* notFollowedBy (satisfy identifierChar))))

identifierChar :: Char -> Bool
identifierChar c = c == '_' || isAscii c && isAlphaNum c

reserved :: [T.Text]
reserved =
  [ "as"
  , "break"
  , "const"
  , "continue"
  , "else"
  , "false"
  , "for"
  , "function"
  , "if"
  , "import"
  , "in"
  , "let"
  , "loop"
  , "package"
  , "namespace"
  , "null"
  , "return"
  , "true"
  , "var"
  , "void"
  , "while"
  ]

identifier :: Parser T.Text
identifier = lexeme do
  name <- T.cons <$> satisfy (\c -> c == '_' || isAsciiLower c || isAsciiUpper c) <*> takeWhileP Nothing identifierChar
  if name `elem` reserved then fail ("reserved identifier " <> T.unpack name) else pure name

expression :: Parser Expr
expression = do
  condition <- disjunction
  option condition do
    operator "?"
    whenTrue <- disjunction
    operator ":"
    Conditional condition whenTrue <$> expression

disjunction :: Parser Expr
disjunction = makeExprParser relation [[InfixL (And <$ operator "&&")], [InfixL (Or <$ operator "||")]]

relation :: Parser Expr
relation =
  makeExprParser
    unary
    [
      [ InfixL (Binary Times <$ operator "*")
      , InfixL (Binary Divide <$ operator "/")
      , InfixL (Binary Remainder <$ operator "%")
      ]
    , [InfixL (Binary Plus <$ operator "+"), InfixL (Binary Minus <$ operator "-")]
    ,
      [ InfixL (Binary LessOrEqual <$ operator "<=")
      , InfixL (Binary GreaterOrEqual <$ operator ">=")
      , InfixL (Binary Less <$ operator "<")
      , InfixL (Binary Greater <$ operator ">")
      , InfixL (Binary Equals <$ operator "==")
      , InfixL (Binary NotEquals <$ operator "!=")
      , InfixL (Binary Within <$ keyword "in")
      ]
    ]

unary :: Parser Expr
unary =
  choice
    [ do
        bangs <- some (operator "!")
        operand <- member
        pure (foldl' (\inner _ -> Not inner) operand bangs)
    , try negativeLiteral
    , do
        minuses <- some (operator "-")
        operand <- member
        pure (foldl' (\inner _ -> Negate inner) operand minuses)
    , member
    ]

negativeLiteral :: Parser Expr
negativeLiteral = do
  operator "-"
  literal <- lexeme (number True)
  postfixes (Literal literal)

member :: Parser Expr
member = primary >>= postfixes

postfixes :: Expr -> Parser Expr
postfixes target =
  option target do
    next <-
      choice
        [ do
            void (symbol ".")
            name <- identifier
            option (Select target name) do
              arguments <- between (symbol "(") (symbol ")") (expression `sepEndBy` symbol ",")
              maybe (fail ("invalid arguments to the " <> T.unpack name <> " macro")) pure (memberCall target name arguments)
        , Index target <$> between (symbol "[") (symbol "]") expression
        , do
            name <- maybe (fail "message construction needs a type name") pure (qualifiedName target)
            initializers <- between (symbol "{") (symbol "}") (initializer `sepEndBy` symbol ",")
            pure (Construct name initializers)
        ]
    postfixes next
 where
  initializer = do
    name <- identifier
    void (symbol ":")
    value <- expression
    pure (name, value)

qualifiedName :: Expr -> Maybe T.Text
qualifiedName = \case
  Ident name -> Just name
  Select inner name -> (<> ("." <> name)) <$> qualifiedName inner
  _ -> Nothing

memberCall :: Expr -> T.Text -> [Expr] -> Maybe Expr
memberCall target name arguments = case (name, arguments) of
  ("all", [Ident variable, body]) -> Just (Loop Every variable target body)
  ("exists", [Ident variable, body]) -> Just (Loop Some variable target body)
  ("exists_one", [Ident variable, body]) -> Just (Loop ExactlyOne variable target body)
  ("filter", [Ident variable, body]) -> Just (Loop Keeping variable target body)
  ("map", [Ident variable, body]) -> Just (Transform variable target Nothing body)
  ("map", [Ident variable, keep, body]) -> Just (Transform variable target (Just keep) body)
  (macro, _)
    | macro `elem` ["all", "exists", "exists_one", "filter", "map"] -> Nothing
    | otherwise -> Just (Member target name arguments)

primary :: Parser Expr
primary =
  choice
    [ between (symbol "(") (symbol ")") expression
    , List <$> between (symbol "[") (symbol "]") (expression `sepEndBy` symbol ",")
    , Entries <$> between (symbol "{") (symbol "}") (entry `sepEndBy` symbol ",")
    , Literal <$> lexeme (try textual)
    , Literal <$> lexeme (number False)
    , Literal (BoolV True) <$ keyword "true"
    , Literal (BoolV False) <$ keyword "false"
    , Literal NullV <$ keyword "null"
    , do
        leading <- option "" ("." <$ symbol ".")
        name <- identifier
        option (Ident (leading <> name)) do
          arguments <- between (symbol "(") (symbol ")") (expression `sepEndBy` symbol ",")
          maybe (fail "invalid argument to has() macro") pure (globalCall name arguments)
    ]
 where
  entry = do
    key <- expression
    void (symbol ":")
    value <- expression
    pure (key, value)

globalCall :: T.Text -> [Expr] -> Maybe Expr
globalCall name arguments = case (name, arguments) of
  ("has", [Select target field]) -> Just (Present target field)
  ("has", _) -> Nothing
  _ -> Just (Call name arguments)

number :: Bool -> Parser Value
number negative = do
  hexadecimal <- option False (True <$ try (char '0' *> char' 'x' <* lookAhead (satisfy isHexDigit)))
  if hexadecimal
    then do
      digits <- takeWhile1P Nothing isHexDigit
      integral (foldl' (\acc digit -> acc * 16 + toInteger (digitValue digit)) 0 (T.unpack digits))
    else do
      whole <- takeWhileP Nothing isDigit
      fraction <- optional (try (char '.' *> takeWhile1P Nothing isDigit))
      exponentText <- optional (try exponentPart)
      case (fraction, exponentText) of
        (Nothing, Nothing)
          | T.null whole -> fail "expected a number"
          | otherwise -> integral (foldl' (\acc digit -> acc * 10 + toInteger (digitValue digit)) 0 (T.unpack whole))
        _ -> do
          let written = (if T.null whole then "0" else whole) <> maybe "" ("." <>) fraction <> fromMaybe "" exponentText
          pure (DoubleV ((if negative then negate else id) (fromRational (decimalRational written))))
 where
  exponentPart = do
    marker <- char' 'e'
    sign <- option "" (T.singleton <$> (char '+' <|> char '-'))
    digits <- takeWhile1P Nothing isDigit
    pure (T.singleton marker <> sign <> digits)
  integral magnitude = do
    unsignedSuffix <- option False (True <$ char' 'u')
    notFollowedBy (satisfy identifierChar)
    let signed = if negative then negate magnitude else magnitude
    if
      | unsignedSuffix && not negative && magnitude <= toInteger (maxBound :: Word64) -> pure (UintV (fromInteger magnitude))
      | unsignedSuffix -> fail "unsigned literal out of range"
      | signed >= toInteger (minBound :: Int64) && signed <= toInteger (maxBound :: Int64) -> pure (IntV (fromInteger signed))
      | otherwise -> fail "integer literal out of range"

decimalRational :: T.Text -> Rational
decimalRational written =
  let (mantissa, exponentText) = T.break (\c -> c == 'e' || c == 'E') written
      (whole, fraction) = T.break (== '.') mantissa
      digits = whole <> T.drop 1 fraction
      scale = T.length (T.drop 1 fraction)
      scale10 = case T.unpack (T.drop 1 exponentText) of
        '-' : rest -> negate (digitsValue rest)
        '+' : rest -> digitsValue rest
        rest -> digitsValue rest
      value = toRational (digitsValue (T.unpack digits))
      power = scale10 - toInteger scale
   in if power >= 0 then value * 10 ^ power else value / 10 ^ negate power
 where
  digitsValue = foldl' (\acc digit -> acc * 10 + toInteger (digitValue digit)) 0

textual :: Parser Value
textual = do
  prefix <- T.toLower <$> takeWhileP Nothing (`T.elem` "rRbB")
  if T.length prefix > 2 || T.length (T.filter (== 'r') prefix) > 1 || T.length (T.filter (== 'b') prefix) > 1
    then fail "invalid string prefix"
    else do
      let raw = 'r' `T.elem` prefix
          bytes = 'b' `T.elem` prefix
      pieces <- quoted raw
      pure
        if bytes
          then BytesV (BS.pack (concatMap pieceBytes pieces))
          else StringV (T.concat (fmap pieceText pieces))

data Piece = Char Char | Byte Word8

pieceBytes :: Piece -> [Word8]
pieceBytes = \case
  Byte byte -> [byte]
  Char c -> BS.unpack (TE.encodeUtf8 (T.singleton c))

pieceText :: Piece -> T.Text
pieceText = \case
  Byte byte -> T.singleton (chr (fromIntegral byte))
  Char c -> T.singleton c

quoted :: Bool -> Parser [Piece]
quoted raw =
  choice
    [ try (string "\"\"\"") *> manyTill (piece True) (string "\"\"\"")
    , try (string "'''") *> manyTill (piece True) (string "'''")
    , char '"' *> manyTill (piece False) (char '"')
    , char '\'' *> manyTill (piece False) (char '\'')
    ]
 where
  piece multiline
    | raw = Char <$> plain multiline
    | otherwise = char '\\' *> escape <|> Char <$> plain multiline
  plain multiline = satisfy (\c -> multiline || not (c `T.elem` "\n\r"))
  escape =
    choice
      [ Char '\a' <$ char 'a'
      , Char '\b' <$ char 'b'
      , Char '\f' <$ char 'f'
      , Char '\n' <$ char 'n'
      , Char '\r' <$ char 'r'
      , Char '\t' <$ char 't'
      , Char '\v' <$ char 'v'
      , Char '"' <$ char '"'
      , Char '\'' <$ char '\''
      , Char '\\' <$ char '\\'
      , Char '`' <$ char '`'
      , Char '?' <$ char '?'
      , Byte . fromIntegral <$> (char' 'x' *> hexDigits 2)
      , char 'u' *> (hexDigits 4 >>= codePoint)
      , char 'U' *> (hexDigits 8 >>= codePoint)
      , do
          leading <- satisfy (`T.elem` "0123")
          rest <- count 2 (satisfy isOctDigit)
          pure (Byte (fromIntegral (foldl' (\acc digit -> acc * 8 + digitValue digit) 0 (leading : rest))))
      ]
  hexDigits n = foldl' (\acc digit -> acc * 16 + digitValue digit) 0 <$> count n (satisfy isHexDigit)
  codePoint value
    | value > 0x10FFFF || value >= 0xD800 && value <= 0xDFFF = fail "invalid code point"
    | otherwise = pure (Char (chr value))
