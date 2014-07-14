{-# LANGUAGE FlexibleContexts #-}
module Lexer (
  whiteSpace,
  atomParen,
  atom,
  variable,
  symbol
)where
import Data.Char (isSpace)
import Control.Applicative ((<$>),(<*>),(*>),(<*))
import Control.Monad
import Text.Parsec

special :: Stream s m Char => ParsecT s u m Char
special = oneOf "+-*/\\^~:.?@#$&<>="

upperPr :: Stream s m Char => ParsecT s u m Char
upperPr = upper <|> char '_'

character :: Stream s m Char => ParsecT s u m Char
character = lower <|> upperPr <|> digit

characterInSingleQuote :: Stream s m Char => ParsecT s u m Char
characterInSingleQuote =
  try (string "''") *> return '\'' <|>
  char '\\' *> noneOf "" <|>
  noneOf "'"

nestedComment :: Stream s m Char => ParsecT s u m ()
nestedComment = string "/*" *> nestedCommentRest where
  nestedCommentRest =
    string "*/" *> return () <|>
    nestedComment *> nestedCommentRest <|>
    anyChar *> nestedCommentRest

lineComment :: Stream s m Char => ParsecT s u m ()
lineComment = string "%" *> lineCommentRest where
  lineCommentRest = char '\n' *> return () <|> anyChar *> lineCommentRest

simpleSpace :: Stream s m Char => ParsecT s u m ()
simpleSpace = skipMany1 (satisfy isSpace)

whiteSpace :: Stream s m Char => ParsecT s u m ()
whiteSpace = skipMany (nestedComment <|> lineComment <|> simpleSpace <?> "")

lexeme :: Stream s m Char => ParsecT s u m a => ParsecT s u m a
lexeme = (<* whiteSpace)

small_atom' :: Stream s m Char => ParsecT s u m String
small_atom' = do
  ch <- lower
  chs <- many character
  return (ch:chs)

special_atom' :: Stream s m Char => [String] -> ParsecT s u m String
special_atom' ex = do
  str <- (:) <$> special <*> many special
  guard $ notElem str ex
  return str

atom' :: Stream s m Char => [String] -> ParsecT s u m String
atom' ex =
  small_atom' <|>
  special_atom' ex <|>
  char '\'' *> many characterInSingleQuote <* char '\'' <|>
  string "!"

atomParen :: Stream s m Char => ParsecT s u m String
atomParen = lexeme $ try $ atom' [] <* char '('

atom :: Stream s m Char => ParsecT s u m String
atom = lexeme $ try $ atom' [",", ".", "|"] <* (try (char '(') *> mzero <|> return ())

variable :: Stream s m Char => ParsecT s u m String
variable = lexeme $ try $ do
  ch <- upperPr
  chs <- many character
  return (ch:chs)

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = lexeme . string
