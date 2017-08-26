{-# LANGUAGE GADTs, DataKinds, EmptyDataDecls, KindSignatures, ScopedTypeVariables #-}

module BTree where

select1 x y lt eq gt
  = case compare x y of { LT -> lt; EQ -> eq; GT -> gt }

select2 x y z xlty xeqy xbtw xeqz xgtz
  = select1 x y xlty xeqy (select1 x z xbtw xeqz xgtz)

t1 a b c = Br (T1 a b c)
t2 a b c d e = Br (T2 a b c d e)

data Foo = Foo

data T n a where
  Br :: N n a -> T (S n) a
  LF :: T Z a

data N n a
  = T1 (T n a) a (T n a)
  | T2 (T n a) a (T n a) a (T n a)

data Nat = Z | S Nat | M | P

data Tree a where
  Tree :: T n a -> Tree a


