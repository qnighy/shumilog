module Eval (
  evalQuery
) where
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Cont
import ListT
import Control.Monad.Error
import Control.Applicative
import qualified Data.Map.Strict as Map
import Terms
import Preterm

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

evalTermsC :: (Me () -> Me ()) -> [Term] -> Me ()
evalTermsC _ [] = return ()
evalTermsC exit (Compound sym args:remain) = do
  env <- lift $ lift get
  case Map.lookup sym (predicates env) of
    Just (NormalPredicateDef clauses _) ->
      evalClauses args clauses *> evalTermsC exit remain
    Just (SpecialPredicateDef pfun) ->
      pfun args *> evalTermsC exit remain
    Just Cut -> exit $ evalTermsC exit remain
    Nothing -> do
      nam <- lift $ lift $ lift $ getsymnameQual sym
      lift $ lift $ lift $ throwError $ "Cannot find predicate " ++ nam
evalTermsC exit (Variable v:remain) = do
  eenv <- get
  case Map.lookup v (substMap eenv) of
    Just t -> evalTermsC exit (t:remain)
    Nothing -> lift $ lift $ lift $
                 throwError "Arguments are not sufficiently instantiated"
evalTermsC _ (Placeholder:_) =
  lift $ lift $ lift $ throwError "Arguments are not sufficiently instantiated"

evalClauses :: [Term] -> [Abstraction ([Term], [Term])] -> Me ()
evalClauses _ [] = mzero
evalClauses pargs (cl:cls) = join $ callCC (\exit -> do
    evalClauseC exit pargs cl <|> evalClauses pargs cls
    return $ return ()
  )

evalClauseC :: (Me () -> Me ()) -> [Term] -> Abstraction ([Term], [Term]) -> Me ()
evalClauseC exit pargs cl = do
  (args, clvalue) <- abstractOut cl
  zipWithM_ unify args pargs
  evalTermsC exit clvalue

evalQuery' :: Abstraction [Term] -> Me ()
evalQuery' q = join $ callCC (\exit -> do
    evalQuery'C exit q
    return $ return ()
  )

evalQuery'C :: (Me () -> Me ()) -> Abstraction [Term] -> Me ()
evalQuery'C exit q = do
  qvalue <- abstractOut q
  evalTermsC exit qvalue

evalQuery :: Abstraction [Term] -> M ()
evalQuery q = do
  flip runContT return $ foldrListTl (\eenv -> do
    let varids = filter (<abstractionSize q) (Map.keys (substMap eenv))
    if length varids == 0 then
      liftIO $ putStrLn $ "true ;"
    else
      forM_ varids (\varid -> do
        liftIO $ putStr $
          Map.findWithDefault ("?" ++ show varid)
                              varid (abstractionNames q) ++ " = "
        preterm <- lift $ showTermA q eenv
                             (fromJust $ Map.lookup varid (substMap eenv))
        liftIO $ putStrLn $ showPreterm preterm ++ " ;")
    return id
   ) (do
     liftIO $ putStrLn "false ."
     liftIO $ putStrLn ""
   ) (flip execStateT emptyEvalEnv $ evalQuery' q)

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

