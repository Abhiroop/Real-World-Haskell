{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables, InstanceSigs#-}
module Continuation where

import Control.Monad(liftM,ap)
import Control.Monad.Trans(MonadTrans(..))

newtype Trampoline m r =
  Trampoline {bounce :: m (Either (Trampoline m r) r)}

instance (Monad m) => Applicative (Trampoline m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Functor (Trampoline m) where
    fmap = liftM

instance Monad m => Monad(Trampoline m) where
  return = Trampoline . return . Right

  (>>=) :: Trampoline m a -> (a -> Trampoline m b) -> Trampoline m b
  Trampoline ma >>= f =
    Trampoline $ do
      v <- ma -- v :: Either (Trampoline m a) a
      case v of
        Right a -> bounce (f a)
        Left tma -> return $ Left $ tma >>= f

   -- t >>= f = Trampoline (bounce t >>= either (return . Left . (>>= f)) (bounce .f))

instance MonadTrans Trampoline where
  lift = Trampoline . liftM Right

pause :: Monad m => Trampoline m ()
pause = Trampoline (return $ Left $ return ())

run :: Monad m => Trampoline m r -> m r
run (Trampoline mr) = do
  v <- mr
  case v of
    Right r -> return r
    Left t -> run t
