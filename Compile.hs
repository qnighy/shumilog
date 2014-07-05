module Compile (
  compile
) where
import Terms
import Data.Array.IArray
import Control.Applicative
import Control.Monad.State.Strict
import qualified Parser as Parser
import qualified Data.Map.Strict as Map

data CompileEnv = CompileEnv {
  clauseDecls :: Map.Map Symbol [Clause]
}

data TermAbstEnv = TermAbstEnv {
  abstSize :: Int,
  abstMap :: Map.Map String Int,
  abstNames :: Map.Map Int String
}

emptyTermAbstEnv :: TermAbstEnv
emptyTermAbstEnv = TermAbstEnv {
  abstSize = 0,
  abstMap = Map.empty,
  abstNames = Map.empty
}

type M = State (Environment, CompileEnv)

getsym :: (String, Int) -> M Symbol
getsym nam = do
  (e, ce) <- get
  case Map.lookup nam (symbolMap e) of
    Just sym -> return sym
    Nothing -> do
      let fresh = freshSymbolID e
      let freshsym = Symbol fresh
      put (e {
        symbolMap = Map.insert nam freshsym $ symbolMap e,
        symbolNames = Map.insert freshsym nam $ symbolNames e,
        freshSymbolID = fresh + 1
      }, ce)
      return freshsym

getabst :: Monad m => String -> StateT TermAbstEnv m Int
getabst nam = do
  e <- get
  case Map.lookup nam (abstMap e) of
    Just idx -> return idx
    Nothing -> do
      let idx = abstSize e
      put $ e {
        abstSize = idx + 1,
        abstMap = Map.insert nam idx $ abstMap e,
        abstNames = Map.insert idx nam $ abstNames e
      }
      return idx

term :: Parser.Term -> StateT TermAbstEnv M Term
term (Parser.Compound nam args) =
  Compound <$> (lift . getsym) (nam, length args) <*> mapM term args
term (Parser.Variable nam) =
  Variable <$> getabst nam

predicate :: Parser.Predicate -> StateT TermAbstEnv M Predicate
predicate (Parser.Predicate nam destr) =
  Predicate <$> (lift . getsym) (nam, length destr) <*> mapM term destr

addClauseDecl :: Parser.Clause -> M ()
addClauseDecl (Parser.Clause pr' prs') = do
  ((pr:prs),tae) <- runStateT (mapM predicate (pr':prs')) emptyTermAbstEnv
  (e, ce) <- get
  let cl = Clause {
    clauseAbstraction = abstSize tae, -- TODO
    clauseAbstractionNames = abstNames tae,
    clauseArgs = predArgs pr,
    clauseValue = prs
  }
  put (e, ce {
    clauseDecls = Map.insertWith (++) (predSymbol pr) [cl] $ clauseDecls ce
  })

compile :: [Parser.Clause] -> Environment -> Environment
compile cl env = fst $ execState compile' (env, empty_env) where
  empty_env = CompileEnv { clauseDecls = Map.empty }
  compile' :: M ()
  compile' = do
    forM_ cl addClauseDecl
    (e, ce) <- get
    let updates = map (\(sym, cls) -> (symbolID sym, reverse cls)) $
                    Map.assocs $ clauseDecls ce
    put (e {
      predicateDecls = (predicateDecls e) // updates
    }, ce)
