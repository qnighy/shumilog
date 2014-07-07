{-# LANGUAGE FlexibleContexts #-}
module Terms (
  Symbol(Symbol, symbolID),
  Term(Compound, Variable, Placeholder),
  shiftTerm,
  Predicate(Predicate, predSymbol, predArgs),
  Clause(Clause, clauseAbstraction, clauseAbstractionNames,
         clauseArgs, clauseValue),
  Query(Query, queryAbstraction, queryAbstractionNames, queryValue),
  Environment(Environment, symbolMap, symbolNames,
              freshSymbolID, predicateDecls),
  empty_env,
  getsym,
  getsymname,
  TermEnv(TermEnv, abstSize, abstMap, abstNames),
  emptyTermEnv,
  showTerm,
  showPredicate,
  showClause
) where
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative

newtype Symbol = Symbol {symbolID :: Int} deriving (Eq, Ord)

data Term = Compound !Symbol ![Term]
          | Variable !Int
          | Placeholder


shiftTerm :: Int -> Term -> Term
shiftTerm s (Compound sym args) = Compound sym (map (shiftTerm s) args)
shiftTerm s (Variable num) = Variable (num + s)
shiftTerm _ Placeholder = Placeholder

data Predicate = Predicate {
  predSymbol :: !Symbol,
  predArgs :: ![Term]
}

data Clause = Clause {
  clauseAbstraction :: !Int,
  clauseAbstractionNames :: !(Map.Map Int String),
  clauseArgs :: ![Term],
  clauseValue :: ![Predicate]
}

data Query = Query {
  queryAbstraction :: !Int,
  queryAbstractionNames :: !(Map.Map Int String),
  queryValue :: ![Predicate]
}

data Environment = Environment {
  symbolMap :: !(Map.Map (String, Int) Symbol),
  symbolNames :: !(Map.Map Symbol (String, Int)),
  freshSymbolID :: !Int,
  predicateDecls :: !(Map.Map Symbol [Clause])
}

empty_env :: Environment
empty_env = Environment {
  symbolMap = Map.empty,
  symbolNames = Map.empty,
  freshSymbolID = 0,
  predicateDecls = Map.empty
}

getsym :: (Functor m, Monad m,
           MonadReader Environment m, MonadError String m) =>
            (String, Int) -> m Symbol
getsym nam = do
  res <- Map.lookup nam <$> (symbolMap <$> ask)
  case res of
    Just x -> return x
    Nothing -> throwError "atom not found"

getsymname :: (Functor m, Monad m, MonadReader Environment m) =>
                Symbol -> m String
getsymname sym = do
  x <- Map.lookup sym <$> (symbolNames <$> ask)
  return $ fst $ fromJust x

data TermEnv = TermEnv {
  abstSize :: !Int,
  abstMap :: !(Map.Map String Int),
  abstNames :: !(Map.Map Int String)
}

emptyTermEnv :: TermEnv
emptyTermEnv = TermEnv {
  abstSize = 0,
  abstMap = Map.empty,
  abstNames = Map.empty
}

showTerm :: (Functor m, Monad m, MonadReader Environment m) => Term -> m String
showTerm (Compound sym args) = do
  nam <- getsymname sym
  argstr <- mapM showTerm args
  return $ nam ++ "(" ++ intercalate ", " argstr ++ ")"
showTerm (Variable varid) = return $ "?" ++ show varid
showTerm Placeholder = return $ "_"

showPredicate :: (Functor m, Monad m, MonadReader Environment m) =>
                   Predicate -> m String
showPredicate (Predicate sym args) = do
  nam <- getsymname sym
  argstr <- mapM showTerm args
  return $ nam ++ "(" ++ intercalate ", " argstr ++ ")"

showClause :: (Functor m, Monad m, MonadReader Environment m) =>
                   Symbol -> Clause -> m String
showClause sym cl = do
  predstr <- showPredicate $ Predicate sym (clauseArgs cl)
  predstrs <- mapM showPredicate (clauseValue cl)
  return $ predstr ++ " :- " ++ intercalate "," predstrs ++ "."
