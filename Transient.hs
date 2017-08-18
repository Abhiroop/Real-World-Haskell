{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Transient where

import Control.Monad (liftM, ap)
import Control.Monad.State.Class

data EventF = forall m b c. EventF (b -> Transient m c)

data Transient m x = Transient (m (Maybe x))

runTrans :: Transient m x -> m (Maybe x)
runTrans (Transient mx) = mx

setEventCont :: MonadState EventF m => (b -> Transient m c) -> m EventF
setEventCont f = do
    f' <- get
    put $ EventF f
    return f'

instance MonadState EventF m => Functor (Transient m) where
  fmap = liftM

instance MonadState EventF m => Applicative (Transient m) where
  pure  = return
  (<*>) = ap

instance MonadState EventF m => Monad (Transient m) where
  return x = Transient $ return $ Just x

  x >>= f = Transient $ do
    setEventCont f
    mk <- runTrans x
    case mk of
      Just k -> runTrans (f k)
      Nothing -> return Nothing
