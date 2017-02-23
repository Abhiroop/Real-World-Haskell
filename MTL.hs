{-# LANGUAGE InstanceSigs #-}

module MTL where

import Control.Applicative

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
