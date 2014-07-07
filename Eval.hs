module Eval where
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Cont
import Control.Monad.List
import Control.Monad.Error
import qualified Data.Map.Strict as Map
import Terms

data SubstEnv = SubstEnv {
  maxVarID :: !Int,
  substMap :: !(Map.Map Int Term)
}

type M r m = ReaderT Environment (StateT SubstEnv (ListT (ErrorT String (ContT r m))))

occurFV :: Int -> Term -> Bool
occurFV v (Compound _ args) = any (occurFV v) args
occurFV v (Variable v') = v == v'
occurFV _ Placeholder = False

unify :: Term -> Term -> M r m ()
unify (Variable v0) t1 = do
  senv <- get
  case Map.lookup v0 (substMap senv) of
    Just t0 -> unify t0 t1
    Nothing ->
      if occurFV v0 t1 then mzero else
        put $ senv {
          substMap = Map.insert v0 t1 (substMap senv)
        }
unify t0 (Variable v1) = do
  senv <- get
  case Map.lookup v1 (substMap senv) of
    Just t1 -> unify t0 t1
    Nothing ->
      if occurFV v1 t0 then mzero else
        put $ senv {
          substMap = Map.insert v1 t0 (substMap senv)
        }
unify (Compound sym0 args0) (Compound sym1 args1) | sym0 == sym1 =
  zipWithM_ unify args0 args1
unify Placeholder _ = return ()
unify _ Placeholder = return ()
unify _ _ = mzero

evalPredicate :: (() -> M r m ()) -> Predicate -> M r m ()
evalPredicate exit p = do
  env <- ask
  case Map.lookup (predSymbol p) (predicateDecls env) of
    Just decls ->
      callCC (\exit' ->
        msum (map (evalClause exit' (predArgs p)) decls)
      )
    Nothing -> throwError "Cannot find predicate"

evalClause :: (() -> M r m ()) -> [Term] -> Clause -> M r m ()
evalClause exit pargs cl = do
  senv <- get
  let shft = maxVarID senv
  put $ senv { maxVarID = shft + clauseAbstraction cl }
  zipWithM_ unify (map (shiftTerm shft) (clauseArgs cl)) pargs
  mapM_ (evalPredicate exit) (clauseValue cl)

evalPredicates_ :: [Predicate] -> M r m ()
evalPredicates_ ps =
  callCC (\exit ->
    mapM_ (evalPredicate exit) ps
  )

evalQuery :: Monad m =>
             Query ->
             ReaderT Environment (ErrorT String m) [Map.Map Int Term]
evalQuery q = do
  env <- ask
  let initial_senv = SubstEnv {
    maxVarID = queryAbstraction q,
    substMap = Map.empty
  }
  lift $ (mapErrorT $ flip runContT $ return)
       $ runListT $ fmap substMap
       $ flip execStateT initial_senv $ flip runReaderT env
       $ evalPredicates_ (queryValue q)
