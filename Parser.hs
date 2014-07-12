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
operatorIn b syms = try $
  do { sym <- atom; guard $ elem sym syms; return sym} <|>
  do { guard $ not b; _ <- symbol ","; return "," }
-- operatorIn _ [] = mzero
-- operatorIn True (",":symtail) = operatorIn False symtail
-- -- operatorIn b (sym:symtail) = symbol sym <|> operatorIn b symtail
-- operatorIn b (sym:symtail) = symbol sym <|> operatorIn b symtail

pCompound1L :: String -> Preterm -> Preterm
pCompound1L sym rhs = PCompound sym [rhs]

pCompound2 :: Preterm -> String -> Preterm -> Preterm
pCompound2 lhs sym rhs = PCompound sym [lhs, rhs]

term :: (MonadState Environment m, Stream s m Char) =>
        Bool -> ParsecT s u m Preterm
term b = termOps b =<< gets operatorInfo

termOps :: (MonadState Environment m, Stream s m Char) =>
           Bool -> [(Int,OperatorType,[String])] -> ParsecT s u m Preterm
termOps _ [] =
  PCompound <$> atomParen <*> term_list <* symbol ")" <|>
  PCompound <$> atom <*> return [] <|>
  PPlaceholder <$ symbol "_" <|>
  PVariable <$> variable <|>
  symbol "(" *> term False <* symbol ")"
termOps b oprs@((_,XF,oprsym):oprtail) = do
  trm <- termOps b oprtail
  syms <- atmost1 $ opEater1 <$> operatorIn b oprsym
  return $ fold_eat trm syms
termOps b oprs@((_,YF,oprsym):oprtail) = do
  trm <- termOps b oprtail
  syms <- many $ opEater1 <$> operatorIn b oprsym
  return $ fold_eat trm syms
termOps b oprs@((_,XFX,oprsym):oprtail) = do
  trm <- termOps b oprtail
  pCompound2 trm <$> operatorIn b oprsym <*> termOps b oprtail <|>
    return trm
termOps b oprs@((_,XFY,oprsym):oprtail) = do
  trm <- termOps b oprtail
  pCompound2 trm <$> operatorIn b oprsym <*> termOps b oprs <|>
    return trm
termOps b oprs@((_,YFX,oprsym):oprtail) = do
  trm <- termOps b oprtail
  syms <- many $ opEater2 <$> operatorIn2 b oprsym oprtail
  return $ fold_eat trm syms
termOps b oprs@((_,FX,oprsym):oprtail) = do
  pCompound1L <$> operatorIn b oprsym <*> termOps b oprtail <|>
    termOps b oprtail
termOps b oprs@((_,FY,oprsym):oprtail) = do
  pCompound1L <$> operatorIn b oprsym <*> termOps b oprs <|>
    termOps b oprtail

atmost1 :: ParsecT s u m a -> ParsecT s u m [a]
atmost1 p = (:[]) <$> p <|> return []

opEater1 :: String -> Preterm -> Preterm
opEater1 oprsym lhs = PCompound oprsym [lhs]

opEater2 :: (String,Preterm) -> Preterm -> Preterm
opEater2 (oprsym,rhs) lhs = PCompound oprsym [lhs, rhs]

fold_eat :: Preterm -> [Preterm -> Preterm] -> Preterm
fold_eat trm syms_eater = foldl (\t sym_eater -> sym_eater t) trm syms_eater

operatorIn2 ::
  (MonadState Environment m, Stream s m Char) =>
  Bool -> [String] -> [(Int,OperatorType,[String])] ->
  ParsecT s u m (String, Preterm)
operatorIn2 b oprsym oprs =
  (,) <$> operatorIn b oprsym <*> termOps b oprs

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
