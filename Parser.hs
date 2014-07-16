{-# LANGUAGE FlexibleContexts #-}
module Parser (
  OperatorType(XF, YF, XFX, XFY, YFX, FY, FX),
  statementOrEnd
) where
import Data.Char (ord)
import Data.List ((\\))
import Text.Parsec
import Control.Applicative ((<$>),(<$),(<*>),(*>),(<*))
import Control.Monad.State.Strict
import Lexer
import Preterm
import Terms

symbolIn :: Stream s m Char => [String] -> [String] -> ParsecT s u m String
symbolIn [] _ = mzero
symbolIn (allowedsym:asyms) syms =
  guard (elem allowedsym syms) *> symbol allowedsym <|> symbolIn asyms syms

operatorIn :: Stream s m Char => [String] -> [String] -> ParsecT s u m String
operatorIn allowedsyms syms = try $
  do { sym <- atom; guard $ elem sym syms; return sym} <|>
  symbolIn allowedsyms syms

pCompound1L :: String -> Preterm -> Preterm
pCompound1L sym rhs = PCompound sym [rhs]

pCompound2 :: Preterm -> String -> Preterm -> Preterm
pCompound2 lhs sym rhs = PCompound sym [lhs, rhs]

term :: (MonadState Environment m, Stream s m Char) =>
        [String] -> ParsecT s u m Preterm
term asyms = termOps asyms =<< gets operatorInfo

termOps :: (MonadState Environment m, Stream s m Char) =>
           [String] -> [(Int,OperatorType,[String])] -> ParsecT s u m Preterm
termOps _ [] =
  PCompound <$> atomParen <*> term_list [",","|"] <* symbol ")" <|>
  PCompound <$> atom <*> return [] <|>
  PPlaceholder <$ symbol "_" <|>
  PVariable <$> variable <|>
  symbol "(" *> term [",", "|"] <* symbol ")" <|>
  symbol "[" *>
    ((return (PCompound "[]" []) <* symbol "]") <|>
     (term_listsyn [",", "|"] <* symbol "]")) <|>
  string_to_intlist <$> pstring <|>
  PInteger <$> intliteral
termOps asyms ((_,XF,oprsym):oprtail) = do
  trm <- termOps asyms oprtail
  syms <- atmost1 $ opEater1 <$> operatorIn asyms oprsym
  return $ fold_eat trm syms
termOps asyms ((_,YF,oprsym):oprtail) = do
  trm <- termOps asyms oprtail
  syms <- many $ opEater1 <$> operatorIn asyms oprsym
  return $ fold_eat trm syms
termOps asyms ((_,XFX,oprsym):oprtail) = do
  trm <- termOps asyms oprtail
  pCompound2 trm <$> operatorIn asyms oprsym <*> termOps asyms oprtail <|>
    return trm
termOps asyms oprs@((_,XFY,oprsym):oprtail) = do
  trm <- termOps asyms oprtail
  pCompound2 trm <$> operatorIn asyms oprsym <*> termOps asyms oprs <|>
    return trm
termOps asyms ((_,YFX,oprsym):oprtail) = do
  trm <- termOps asyms oprtail
  syms <- many $ opEater2 <$> operatorIn2 asyms oprsym oprtail
  return $ fold_eat trm syms
termOps asyms ((_,FX,oprsym):oprtail) = do
  pCompound1L <$> operatorIn asyms oprsym <*> termOps asyms oprtail <|>
    termOps asyms oprtail
termOps asyms oprs@((_,FY,oprsym):oprtail) = do
  pCompound1L <$> operatorIn asyms oprsym <*> termOps asyms oprs <|>
    termOps asyms oprtail

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
  [String] -> [String] -> [(Int,OperatorType,[String])] ->
  ParsecT s u m (String, Preterm)
operatorIn2 asyms oprsym oprs =
  (,) <$> operatorIn asyms oprsym <*> termOps asyms oprs

term_list :: (MonadState Environment m, Stream s m Char) =>
             [String] -> ParsecT s u m [Preterm]
term_list asyms =
  (:) <$> term (asyms \\ [","]) <*> (symbol "," *> term_list asyms <|> return [])

term_listsyn :: (MonadState Environment m, Stream s m Char) =>
             [String] -> ParsecT s u m Preterm
term_listsyn asyms = do
  lst <- term_list (asyms \\ ["|"])
  (do
    _ <- symbol "|"
    trm <- term (asyms \\ [",", "|"])
    return $ foldr (\x y -> PCompound "." [x, y]) trm lst) <|>
    (return $ foldr (\x y -> PCompound "." [x, y]) (PCompound "[]" []) lst)

statementOrEnd :: (MonadState Environment m, Stream s m Char) =>
                  ParsecT s u m (Maybe Preterm)
statementOrEnd =
  Just <$> term [",", "|"] <* symbol "." <|>
  Nothing <$ eof

string_to_intlist :: String -> Preterm
string_to_intlist [] = PCompound "[]" []
string_to_intlist (ch:chs) =
  PCompound "." [PInteger $ toInteger $ ord ch, string_to_intlist chs]

-- program :: Stream s m Char => ParsecT s u m Program
-- program = Program <$ whiteSpace <*> many clause <*> many query <* eof
