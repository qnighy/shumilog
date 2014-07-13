{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module ContT2 where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

newtype ContT2 r m a = ContT2 {
  runContT2 :: (m a -> m r) -> m r
}

evalContT2 :: ContT2 r m r -> m r
evalContT2 m = runContT2 m id

callCC2 :: ((ContT2 r m a -> ContT2 r m b) -> ContT2 r m a) -> ContT2 r m a
callCC2 f = ContT2 $ \c -> runContT2 (f (\x -> ContT2 $ \_ -> runContT2 x c)) c

instance Functor m => Functor (ContT2 r m) where
  fmap f m = ContT2 $ \c -> runContT2 m (c . fmap f)

instance Applicative f => Applicative (ContT2 r f) where
  pure x = ContT2 ($ pure x)
  f <*> v = ContT2 $ \c -> runContT2 f $ \g -> runContT2 v (c . (g <*>))

instance Monad m => Monad (ContT2 r m) where
  return x = ContT2 ($ return x)
  m >>= k = ContT2 $ \c -> runContT2 m (>>= (\x -> runContT2 (k x) c))

instance MonadTrans (ContT2 r) where
  lift m = ContT2 ($ m)

instance MonadIO m => MonadIO (ContT2 r m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (ContT2 r m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadPlus m => MonadPlus (ContT2 r m) where
  mzero = lift mzero
  mplus x y = ContT2 $ \cont ->
    runContT2 x (\z -> runContT2 y (\w -> cont $ mplus z w))

instance Alternative m => Alternative (ContT2 r m) where
  empty = ContT2 ($ empty)
  x <|> y = ContT2 $ \cont ->
    runContT2 x (\z -> runContT2 y (\w -> cont $ z <|> w))
