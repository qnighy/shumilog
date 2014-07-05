module Terms where
import Data.Array.IArray
import qualified Data.Map.Strict as Map

newtype Symbol = Symbol {symbolID :: Int} deriving (Eq, Ord)

data Term = Compound !Symbol ![Term]
          | Variable Int

data Predicate = Predicate {
  predSymbol :: !Symbol,
  predArgs :: ![Term]
}

data Clause = Clause {
  clauseAbstraction :: !Int,
  clauseAbstractionNames :: Map.Map Int String,
  clauseArgs :: [Term],
  clauseValue :: [Predicate]
}

data Environment = Environment {
  symbolMap :: Map.Map (String, Int) Symbol,
  symbolNames :: Map.Map Symbol (String, Int),
  freshSymbolID :: Int,
  predicateDecls :: Array Int [Clause]
}
