{-# LANGUAGE FlexibleContexts #-}
module Compile (
  compile,
  compileQuery
) where
import Terms
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Error
import qualified Parser as Parser
import qualified Data.Map.Strict as Map

data CompileEnv = CompileEnv {
  clauseDecls :: Map.Map Symbol [Clause]
}
emptyCompileEnv :: CompileEnv
emptyCompileEnv = CompileEnv { clauseDecls = Map.empty }

type M = StateT CompileEnv (State Environment)
type M2 = StateT TermEnv M

getsymState :: (Monad m, MonadState Environment m) => (String, Int) -> m Symbol
getsymState nam = do
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

getvar :: (Monad m, MonadState TermEnv m) => String -> m Int
getvar nam = do
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

term :: (Functor m, Applicative m, Monad m,
         MonadReader Environment m, MonadState TermEnv m,
         MonadError String m) =>
        Parser.Term -> m Term
term (Parser.Compound nam args) =
  Compound <$> getsym (nam, length args)
           <*> mapM term args
term (Parser.Variable nam) =
  Variable <$> getvar nam
term Parser.Placeholder = pure Placeholder

termState :: Parser.Term -> M2 Term
termState (Parser.Compound nam args) =
  Compound <$> (lift . lift . getsymState) (nam, length args)
           <*> mapM termState args
termState (Parser.Variable nam) =
  Variable <$> getvar nam
termState Parser.Placeholder = pure Placeholder

predicate :: (Functor m, Applicative m, Monad m,
              MonadReader Environment m, MonadError String m,
              MonadState TermEnv m) =>
             Parser.Predicate -> m Predicate
predicate (Parser.Predicate nam destr) =
  Predicate <$> getsym (nam, length destr)
            <*> mapM term destr

predicateState :: Parser.Predicate -> M2 Predicate
predicateState (Parser.Predicate nam destr) =
  Predicate <$> (lift . lift . getsymState) (nam, length destr)
            <*> mapM termState destr

addClauseDecl :: Parser.Clause -> M ()
addClauseDecl (Parser.Clause pr' prs') = do
  ((pr:prs),tae) <- runStateT (mapM predicateState (pr':prs')) emptyTermEnv
  ce <- get
  let cl = Clause {
    clauseAbstraction = abstSize tae,
    clauseAbstractionNames = abstNames tae,
    clauseArgs = predArgs pr,
    clauseValue = prs
  }
  put $ ce {
    clauseDecls = Map.insertWith (++) (predSymbol pr) [cl] $ clauseDecls ce
  }

compile :: [Parser.Clause] -> Environment -> Environment
compile cl env = execState (evalStateT compile' emptyCompileEnv) env where
  compile' :: M ()
  compile' = do
    forM_ cl addClauseDecl
    ce <- get
    lift $ modify (\e -> e {
      predicateDecls = foldl (\m (sym, cls) -> Map.insert sym cls m)
                             (predicateDecls e)
                             (Map.assocs $ clauseDecls ce)
    })

compileQuery :: (Functor m, Monad m, MonadReader Environment m,
                  MonadError String m) => Parser.Query -> m Query
compileQuery (Parser.Query prs') = do
  (prs,tae) <- runStateT (mapM predicate prs') emptyTermEnv
  return $ Query {
    queryAbstraction = abstSize tae,
    queryAbstractionNames = abstNames tae,
    queryValue = prs
  }

