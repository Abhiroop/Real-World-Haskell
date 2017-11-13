{-# LANGUAGE RankNTypes #-}
module MoreIndexed where

class IxMonad m where
  ipure :: forall a x. a -> m x x a
  ibind :: forall a b x y z. m x y a -> (a -> m y z b) -> m x z b
