{-# LANGUAGE FlexibleContexts #-}
module Compile (
  compileStatement,
  -- compileQuery
) where
import Terms
import Preterm
import Eval
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Error
import qualified Data.Map.Strict as Map

type M2 = StateT TermEnv M

getvar :: String -> StateT TermEnv M Int
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

compileTerm :: Preterm -> M2 Term
compileTerm (PCompound nam args) =
  Compound <$> (lift . getsym) (nam, length args)
           <*> mapM compileTerm args
compileTerm (PVariable nam) =
  Variable <$> getvar nam
compileTerm (PInteger i) = return $ TInteger i
compileTerm (PFloat f) = return $ TFloat f
compileTerm PPlaceholder = pure Placeholder

compileTermList :: Preterm -> M2 [Term]
compileTermList (PCompound "," [t0, t1]) =
  (++) <$> compileTermList t0 <*> compileTermList t1
compileTermList t = (:[]) <$> compileTerm t

compileStatement' :: Preterm -> M2 (Symbol, ([Term], [Term]))
compileStatement' (PCompound ":-" [t0, t1]) =
  case t0 of
    PCompound t0nam t0args -> do
      t0namc <- lift $ getsym (t0nam, length t0args)
      t0argsc <- mapM compileTerm t0args
      t1c <- compileTermList t1
      return (t0namc, (t0argsc, t1c))
    _ -> throwError "Arguments are not sufficiently instantiated"
compileStatement' (PCompound nam args) = do
  namc <- lift $ getsym (nam, length args)
  argsc <- mapM compileTerm args
  return (namc, (argsc, []))
compileStatement' _ =
  throwError "Arguments are not sufficiently instantiated"

addToPredicateDef ::
  Symbol -> Abstraction ([Term], [Term]) -> M ()
addToPredicateDef nam defn = do
  env <- get
  case
    Map.findWithDefault (NormalPredicateDef [] []) nam
      (predicates env) of
    NormalPredicateDef _ rclauses ->
      let rclauses' = defn : rclauses in
      put $ env {
        predicates =
          Map.insert nam (NormalPredicateDef (reverse rclauses') rclauses')
            (predicates env)
      }
    _ -> throwError "You can't modify this predicate."

compileStatement :: Preterm -> M ()
compileStatement (PCompound "?-" [t]) = do
  (terms, tenv) <- runStateT (compileTermList t) emptyTermEnv
  let terms' = Abstraction {
    abstractionSize = abstSize tenv,
    abstractionNames = abstNames tenv,
    abstractionValue = terms
  }
  evalQuery terms'
compileStatement t = do
  ((sym, defn), tenv) <- runStateT (compileStatement' t) emptyTermEnv
  let defn' = Abstraction {
    abstractionSize = abstSize tenv,
    abstractionNames = abstNames tenv,
    abstractionValue = defn
  }
  addToPredicateDef sym defn'

-- predicate :: (Functor m, Applicative m, Monad m,
--               MonadReader Environment m, MonadError String m,
--               MonadState TermEnv m) =>
--              Preterm -> m Predicate
-- predicate (PCompound nam destr) =
--   Predicate <$> getsym (nam, length destr)
--             <*> mapM term destr
-- 
-- predicateState :: Parser.Predicate -> M2 Predicate
-- predicateState (Parser.Predicate nam destr) =
--   Predicate <$> (lift . lift . getsymState) (nam, length destr)
--             <*> mapM termState destr
-- 
-- addClauseDecl :: Parser.Clause -> M ()
-- addClauseDecl (Parser.Clause pr' prs') = do
--   ((pr:prs),tae) <- runStateT (mapM predicateState (pr':prs')) emptyTermEnv
--   ce <- get
--   let cl = Clause {
--     clauseAbstraction = abstSize tae,
--     clauseAbstractionNames = abstNames tae,
--     clauseArgs = predArgs pr,
--     clauseValue = prs
--   }
--   put $ ce {
--     clauseDecls = Map.insertWith (++) (predSymbol pr) [cl] $ clauseDecls ce
--   }
-- 
-- compile :: [Parser.Clause] -> Environment -> Environment
-- compile cl env = execState (evalStateT compile' emptyCompileEnv) env where
--   compile' :: M ()
--   compile' = do
--     forM_ cl addClauseDecl
--     ce <- get
--     lift $ modify (\e -> e {
--       predicateDecls = foldl (\m (sym, cls) -> Map.insert sym cls m)
--                              (predicateDecls e)
--                              (Map.assocs $ clauseDecls ce)
--     })
-- 
-- compileQuery :: (Functor m, Monad m, MonadReader Environment m,
--                   MonadError String m) => Parser.Query -> m Query
-- compileQuery (Parser.Query prs') = do
--   (prs,tae) <- runStateT (mapM predicate prs') emptyTermEnv
--   return $ Query {
--     queryAbstraction = abstSize tae,
--     queryAbstractionNames = abstNames tae,
--     queryValue = prs
--   }

