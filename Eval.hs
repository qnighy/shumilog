module Eval (
  evalQuery
) where
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
-- import Control.Monad.Cont
import Control.Monad.List
import Control.Monad.Error
import Control.Applicative
import qualified Data.Map.Strict as Map
import Terms
import Preterm

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
unify Placeholder _ = return ()
unify _ Placeholder = return ()
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

evalQuery :: Abstraction [Term] -> M ()
evalQuery q = do
  lst <- runListT $ flip execStateT emptyEvalEnv $ evalQuery' q
  -- liftIO $ putStrLn $ (show $ length lst) ++ " answers"
  forM_ lst (\eenv ->
    let varids = filter (<abstractionSize q) (Map.keys (substMap eenv)) in
    if length varids == 0 then
      liftIO $ putStrLn $ "true ;"
    else
      forM_ varids (\varid -> do
        liftIO $ putStr $
          Map.findWithDefault ("?" ++ show varid)
                              varid (abstractionNames q) ++ " = "
        preterm <- showTermA q eenv
                             (fromJust $ Map.lookup varid (substMap eenv))
        liftIO $ putStrLn $ showPreterm preterm ++ " ;"))
  liftIO $ putStrLn $ "false ."

showTermA :: Abstraction a -> EvalEnvironment -> Term -> M Preterm
showTermA a eenv (Compound sym args) =
  PCompound <$> getsymname sym <*> mapM (showTermA a eenv) args
showTermA a eenv (Variable varid) =
  case Map.lookup varid (substMap eenv) of
    Just t -> showTermA a eenv t
    Nothing -> PVariable <$>
      if varid < abstractionSize a then
        return $ Map.findWithDefault ("?" ++ show varid)
                                     varid (abstractionNames a)
      else
        return $ "?" ++ show varid
showTermA _ _ Placeholder = return PPlaceholder

