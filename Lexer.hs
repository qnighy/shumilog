{-# LANGUAGE FlexibleContexts #-}
module Lexer (
  whiteSpace,
  atom',
  atom,
  variable,
  symbol
)where
import Data.Char (isSpace)
import Control.Applicative ((<$>),(*>),(<*),pure)
import qualified Control.Applicative as A
import Text.Parsec

special :: Stream s m Char => ParsecT s u m Char
special = oneOf "+-*/\\^~:.?@#$&"

upperPr :: Stream s m Char => ParsecT s u m Char
upperPr = upper <|> char '_'

character :: Stream s m Char => ParsecT s u m Char
character = lower <|> upperPr <|> digit <|> special

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
small_atom' = try $ do
  ch <- lower
  chs <- many character
  return (ch:chs)

atom' :: Stream s m Char => ParsecT s u m String
atom' =
  small_atom' <|>
  char '\'' *> many character <* char '\''

atom :: Stream s m Char => ParsecT s u m String
atom = lexeme atom'

variable :: Stream s m Char => ParsecT s u m String
variable = lexeme $ try $ do
  ch <- upperPr
  chs <- many character
  return (ch:chs)

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = lexeme . string
