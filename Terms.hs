module Terms where
import Data.Array.IArray
import qualified Data.Map as Map

newtype Symbol = Symbol Int

data Term = Compound !Symbol ![Term]
          | Variable Int

data Predicate = Predicate !Symbol ![Term]

data Clause = Clause !Int [Term] [Predicate]

data Environment = Environment {
  symbolMap :: Map.Map (String, Int) Symbol,
  symbolNames :: Array Int (String, Int),
  predicateDecls :: Array Int [Clause]
}
