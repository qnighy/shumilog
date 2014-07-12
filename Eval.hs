module Eval (
  evalQuery
) where
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
-- import Control.Monad.Cont
import Control.Monad.List
import Control.Monad.Error
import qualified Data.Map.Strict as Map
import Terms

data EvalEnvironment = EvalEnvironment {
  maxVarID :: !Int,
  substMap :: !(Map.Map Int Term)
}

emptyEvalEnv :: EvalEnvironment
emptyEvalEnv = EvalEnvironment {
  maxVarID = 0,
  substMap = Map.empty
}

type Me = StateT EvalEnvironment (ListT M)

abstractOut :: AbstractableElement a => Abstraction a -> Me a
abstractOut x = do
  eenv <- get
  let ret = shiftElement (maxVarID eenv) (abstractionValue x)
  put $ eenv { maxVarID = maxVarID eenv + abstractionSize x }
  return ret

occurFV :: Int -> Term -> Bool
occurFV v (Compound _ args) = any (occurFV v) args
occurFV v (Variable v') = v == v'
occurFV _ Placeholder = False

unify :: Term -> Term -> Me ()
unify (Variable v0) t1 = do
  eenv <- get
  case Map.lookup v0 (substMap eenv) of
    Just t0 -> unify t0 t1
    Nothing ->
      if occurFV v0 t1 then mzero else
        put $ eenv {
          substMap = Map.insert v0 t1 (substMap eenv)
        }
unify t0 (Variable v1) = do
  eenv <- get
  case Map.lookup v1 (substMap eenv) of
    Just t1 -> unify t0 t1
    Nothing ->
      if occurFV v1 t0 then mzero else
        put $ eenv {
          substMap = Map.insert v1 t0 (substMap eenv)
        }
unify (Compound sym0 args0) (Compound sym1 args1) | sym0 == sym1 =
  zipWithM_ unify args0 args1
unify Placeholder _ = return ()
unify _ Placeholder = return ()
unify _ _ = mzero

evalTerm :: Term -> Me ()
evalTerm (Compound sym args) = do
  env <- lift get
  case Map.lookup sym (predicates env) of
    Just (NormalPredicateDef clauses _) ->
      msum (map (evalClause args) clauses)
    Just (SpecialPredicateDef) -> throwError "TODO: SpecialPredicateDef"
    Nothing -> throwError "Cannot find predicate"
evalTerm (Variable v) = do
  eenv <- get
  case Map.lookup v (substMap eenv) of
    Just t -> evalTerm t
    Nothing -> throwError "Arguments are not sufficiently instantiated"
evalTerm Placeholder = throwError "Arguments are not sufficiently instantiated"

evalClause :: [Term] -> Abstraction ([Term], [Term]) -> Me ()
evalClause pargs cl = do
  (args, clvalue) <- abstractOut cl
  zipWithM_ unify args pargs
  mapM_ evalTerm clvalue

evalQuery' :: Abstraction [Term] -> Me ()
evalQuery' q = do
  qvalue <- abstractOut q
  mapM_ evalTerm qvalue

-- type Me = StateT EvalEnvironment (ListT M)

evalQuery :: Abstraction [Term] -> M ()
evalQuery q = do
  lst <- runListT $ flip execStateT emptyEvalEnv $ evalQuery' q
  liftIO $ putStrLn $ (show $ length lst) ++ " answers"

-- evalPredicates_ :: [Predicate] -> M ()
-- evalPredicates_ ps = mapM_ evalPredicate ps

-- evalQuery :: Monad m =>
--              Query ->
--              ReaderT Environment (ErrorT String m) [Map.Map Int Term]
-- evalQuery q = do
--   env <- ask
--   let initial_senv = SubstEnv {
--     maxVarID = queryAbstraction q,
--     substMap = Map.empty
--   }
--   lift $ (mapErrorT $ flip runContT $ return)
--        $ runListT $ fmap substMap
--        $ flip execStateT initial_senv $ flip runReaderT env
--        $ evalPredicates_ (queryValue q)
