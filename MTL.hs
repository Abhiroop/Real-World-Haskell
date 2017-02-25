{-# LANGUAGE InstanceSigs #-}

module MTL where

import Control.Applicative
import Data.Bifunctor

newtype Identity a = Identity {runIdentity :: a} deriving (Eq,Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity  where
  return = pure
  (Identity a) >>= f = f a

----------------------------------------------------------------------------

newtype Compose f g a = Compose {getCompose :: f (g a)} deriving (Eq,Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap.fmap) f fga

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ (pure.pure) x

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose g) = Compose $ (liftA2 (<*>)) f g
-----------------------------------------------------------------------------

newtype IdentityT f a = IdentityT {runIdentityT :: f a} deriving (Eq,Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)

  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f=
    IdentityT $ ma >>= runIdentityT.f
-------------------------------------------------------------------------------

newtype MaybeT m a =
  MaybeT {runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap.fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT $ (pure.pure) x

  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (liftA2 (<*>)) fab mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT ma >>= f =
    MaybeT $ do
     v <- ma
     case v of
       Nothing -> return Nothing
       Just y  -> runMaybeT (f y)
----------------------------------------------------------------------------------

-- EitherT

---------------------------------------------------------------------------------

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ (fmap.fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where

  pure a = ReaderT (pure (pure a))

  (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ liftA2 (<*>) fmab rma

instance (Monad m) => Monad (ReaderT r m) where

  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r
-------------------------------------------------------------------------------

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where

  fmap :: (a -> b)-> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $
    \s -> let ma = sma s -- ma :: m (a,s)
          in  fmap (first f) ma -- Eg: first (+3) (1,2) = (4,2) first is from Bifunctor

instance (Monad m) => Applicative (StateT s m) where

  pure a = StateT $ \s -> pure (a,s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT smab <*> StateT sma = StateT $
    \s -> do
      (ab,s') <- smab s
      (a,s'')  <- sma s'
      return (ab a,s'')

instance (Monad m) => Monad (StateT s m) where

  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= asmb = StateT $
    \s -> do
      (a,s')  <- sma s
      (runStateT $ asmb a) s'
