{-# LANGUAGE FlexibleContexts #-}
module Terms (
  M,
  OperatorType(XF, YF, XFX, XFY, YFX, FY, FX),
  Symbol(Symbol, symbolID),
  AbstractableElement,
  shiftElement,
  Term(Compound, Variable, Placeholder),
  Abstraction(Abstraction, abstractionSize, abstractionNames, abstractionValue),
  PredicateDef(NormalPredicateDef,SpecialPredicateDef, Cut),
  Environment(Environment, symbolMap, symbolNames,
              freshSymbolID, predicates, operatorInfo),
  empty_env,
  initialize_env,
  getsym,
  getsymname,
  TermEnv(TermEnv, abstSize, abstMap, abstNames),
  emptyTermEnv,
  showTerm,
  EvalEnvironment(EvalEnvironment, maxVarID, substMap),
  emptyEvalEnv,
  Me
) where
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.List
import Control.Monad.Error
import Control.Applicative
import ContT2

type M = ErrorT String (StateT Environment IO)

data OperatorType = XF | YF | XFX | XFY | YFX | FY | FX deriving (Eq, Show)

newtype Symbol = Symbol {symbolID :: Int} deriving (Eq, Ord)

class AbstractableElement a where
  shiftElement :: Int -> a -> a

instance AbstractableElement a => AbstractableElement [a] where
  shiftElement s = map $ shiftElement s

instance (AbstractableElement a, AbstractableElement b) => AbstractableElement (a, b) where
  shiftElement s (x, y) = (shiftElement s x, shiftElement s y)

data Abstraction a = Abstraction {
  abstractionSize :: !Int,
  abstractionNames :: !(Map.Map Int String),
  abstractionValue :: a
}

data Term = Compound !Symbol ![Term]
          | Variable !Int
          | Placeholder

instance AbstractableElement Term where
  shiftElement s (Compound sym args) = Compound sym (map (shiftElement s) args)
  shiftElement s (Variable num) = Variable (num + s)
  shiftElement _ Placeholder = Placeholder

data PredicateDef = NormalPredicateDef
                      [Abstraction ([Term], [Term])]
                      [Abstraction ([Term], [Term])]
                  | SpecialPredicateDef ([Term] -> Me ())
                  | Cut

data Environment = Environment {
  symbolMap :: !(Map.Map (String, Int) Symbol),
  symbolNames :: !(Map.Map Symbol (String, Int)),
  freshSymbolID :: !Int,
  predicates :: !(Map.Map Symbol PredicateDef),
  operatorInfo :: [(Int,OperatorType,[String])]
}

foo = [
  (1200, XFX, ["-->", ":-"]),
  (1200, FX, [":-", "?-"]),
  (1150, FX, ["dynamic", "discontiguous", "initialization", "meta_predicate", "module_transparent", "multifile", "thread_local", "volatile"]),
  (1100, XFY, [";", "|"]),
  (1050, XFY, ["->", "*->"]),
  (1000, XFY, [","]),
  (990, XFX, [":="]),
  (900, FY, ["\\+"]),
  (900, FX, ["~"]),
  (700, XFX, ["<", "=", "=..", "=@=", "=:=", "=<", "==", "=\\=", ">", ">=", "@<", "@=<", "@>", "@>=", "\\=", "\\==", "is", ">:<", ":<"]),
  (600, XFY, [":"]),
  (500, YFX, ["+", "-", "/\\", "\\/", "xor"]),
  (500, FX, ["?"]),
  (400, YFX, ["*", "/", "//", "rdiv", "<<", ">>", "mod", "rem"]),
  (200, XFX, ["**"]),
  (200, XFY, ["^"]),
  (200, FY, ["+", "-", "\\"])]

empty_env :: Environment
empty_env = Environment {
  symbolMap = Map.empty,
  symbolNames = Map.empty,
  freshSymbolID = 0,
  predicates = Map.empty,
  operatorInfo = foo
}

replacePredicateDef ::
  (String, Int) -> PredicateDef -> M ()
replacePredicateDef nam defn = do
  nam' <- getsym nam
  env <- get
  put $ env {
    predicates =
      Map.insert nam' defn (predicates env)
  }

replaceSpecialPredicateDef ::
  (String, Int) -> ([Term] -> Me ()) -> M ()
replaceSpecialPredicateDef nam defn =
  replacePredicateDef nam (SpecialPredicateDef defn)

initialize_env :: M ()
initialize_env = do
  replaceSpecialPredicateDef ("fail", 0) (\_ -> liftIO (putStrLn "foo") *> mzero)
  replacePredicateDef ("!", 0) Cut

getsym :: (String, Int) -> M Symbol
getsym nam = do
  e <- get
  case Map.lookup nam (symbolMap e) of
    Just sym -> return sym
    Nothing -> do
      let fresh = freshSymbolID e
      let freshsym = Symbol fresh
      put $ e {
        symbolMap = Map.insert nam freshsym $ symbolMap e,
        symbolNames = Map.insert freshsym nam $ symbolNames e,
        freshSymbolID = fresh + 1
      }
      return freshsym

getsymname :: Symbol -> M String
getsymname sym = do
  x <- Map.lookup sym <$> (symbolNames <$> get)
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

showTerm :: Term -> M String
showTerm (Compound sym args) = do
  nam <- getsymname sym
  argstr <- mapM showTerm args
  return $ nam ++ "(" ++ intercalate ", " argstr ++ ")"
showTerm (Variable varid) = return $ "?" ++ show varid
showTerm Placeholder = return $ "_"

-- These things should be in Eval.hs

data EvalEnvironment = EvalEnvironment {
  maxVarID :: !Int,
  substMap :: !(Map.Map Int Term)
}

emptyEvalEnv :: EvalEnvironment
emptyEvalEnv = EvalEnvironment {
  maxVarID = 0,
  substMap = Map.empty
}

type Me = (StateT EvalEnvironment (ListT (ContT2 [EvalEnvironment] M)))
