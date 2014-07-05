{-# LANGUAGE FlexibleContexts #-}
module Parser (
  Term(Compound,Variable),
  Predicate(Predicate),
  Clause(Clause),
  Query(Query),
  Program(Program),
  program
) where
import Text.Parsec
import Control.Applicative ((<$>),(<$),(<*>),(*>),(<*),pure)
import qualified Text.Parsec.Token as TP
import Text.Parsec.String
import Lexer

data Term = Compound String [Term]
          | Variable String
            deriving (Show, Eq)

data Predicate = Predicate String [Term] deriving (Show, Eq)

data Clause = Clause Predicate [Predicate] deriving Show

data Query = Query [Predicate] deriving Show

data Program = Program [Clause] [Query] deriving Show

term :: Stream s m Char => ParsecT s u m Term
term =
  Compound <$> atom' <*> (term_args <|> whiteSpace *> return []) <|>
  Variable <$> variable

term_args :: Stream s m Char => ParsecT s u m [Term]
term_args = symbol "(" *> term_args_remain where
  term_args_remain = (:) <$> term <*>
    (symbol ")" *> return [] <|> symbol "," *> term_args_remain)

predicate :: Stream s m Char => ParsecT s u m Predicate
predicate = Predicate <$> atom' <*> (term_args <|> whiteSpace *> return [])

predicate_list :: Stream s m Char => ParsecT s u m [Predicate]
predicate_list = (:) <$> predicate <*>
  (symbol "," *> predicate_list <|> return [])

clause :: Stream s m Char => ParsecT s u m Clause
clause = Clause <$> predicate <*>
  (symbol "." *> return [] <|>
   symbol ":-" *> predicate_list <* symbol ".")

query :: Stream s m Char => ParsecT s u m Query
query = Query <$ symbol "?-" <*> predicate_list <* symbol "."

program :: Stream s m Char => ParsecT s u m Program
program = Program <$ whiteSpace <*> many clause <*> many query <* eof
