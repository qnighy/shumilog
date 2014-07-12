{-# LANGUAGE FlexibleContexts #-}
module Parser (
  OperatorType(XF, YF, XFX, XFY, YFX, FY, FX),
  statementOrEnd
) where
import Text.Parsec
import Control.Applicative ((<$>),(<$),(<*>),(*>),(<*))
import Control.Monad.State.Strict
import Lexer
import Preterm
import Terms

operatorIn :: Stream s m Char => Bool -> [String] -> ParsecT s u m String
operatorIn _ [] = mzero
operatorIn True (",":symtail) = operatorIn False symtail
operatorIn b (sym:symtail) = symbol sym <|> operatorIn b symtail

term :: (MonadState Environment m, Stream s m Char) =>
        Bool -> ParsecT s u m Preterm
term b = termOps b =<< gets operatorInfo

termOps :: (MonadState Environment m, Stream s m Char) =>
           Bool -> [(Int,OperatorType,[String])] -> ParsecT s u m Preterm
termOps _ [] =
  PCompound <$> atom' <*> (term_args <|> whiteSpace *> return []) <|>
  PPlaceholder <$ symbol "_" <|>
  PVariable <$> variable <|>
  symbol "(" *> term False <* symbol ")"
termOps b oprs@((_,XF,oprsym):oprtail) = do
  trm <- termOps b oprtail
  flip PCompound [trm] <$> operatorIn b oprsym <|> return trm
termOps b oprs@((_,YF,oprsym):oprtail) = do
  trm <- termOps b oprtail
  syms <- many $ operatorIn b oprsym
  return (foldl (\t sym -> PCompound sym [t]) trm syms)
-- TODO: other operator type
termOps b (_:oprtail) = termOps b oprtail

term_args :: (MonadState Environment m, Stream s m Char) =>
             ParsecT s u m [Preterm]
term_args = symbol "(" *> term_list <* symbol ")"

term_list :: (MonadState Environment m, Stream s m Char) =>
             ParsecT s u m [Preterm]
term_list = (:) <$> term True <*> (symbol "," *> term_list <|> return [])

statementOrEnd :: (MonadState Environment m, Stream s m Char) =>
                  ParsecT s u m (Maybe Preterm)
statementOrEnd =
  Just <$> term False <* symbol "." <|>
  Nothing <$ eof

-- program :: Stream s m Char => ParsecT s u m Program
-- program = Program <$ whiteSpace <*> many clause <*> many query <* eof
