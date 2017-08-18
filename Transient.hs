{-# LANGUAGE ExistentialQuantification #-}
module Transient where

import Control.Monad (liftM, ap)
import Control.Monad.State.Class

data EventF = forall m b c. EventF (b -> Transient m c)
data Transient m x = Transient (m (Maybe x))

-- instance Functor (Transient m) where
--   fmap = liftM

-- instance Applicative (Transient m) where
--   pure  = return
--   (<*>) = ap

-- instance MonadState m => Monad (Transient m) where
--   return x = Transient $ return $ Just x
--   x >>= f = undefined
