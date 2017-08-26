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

type Keep t n a = T n a -> t
type Push t n a = T n a -> a -> T n a -> t

insert :: forall a . Ord a => a -> Tree a -> Tree a
insert  x (Tree tree) = ins tree Tree (\a b c -> Tree (t1 a b c))
  where
    ins :: forall n t . T n a -> Keep t n a -> Push t n a -> t
    ins LF = \keep push -> push LF x LF

    ins (Br n) = i n
      where
        i :: forall p m. (S p ~ m) => N p a -> Keep t m a -> Push t m a -> t
        i (T2 a b c d e) keep push = select2 x b d xltb xeqb xbtw xeqd xgtd
          where
            xltb = ins a (\k -> keep (t2 k b c d e)) (\p q r -> push (t1 p q r) b (t1 c d e))
            xbtw = ins c (\k -> keep (t2 a b k d e)) (\p q r -> push (t1 a b p) q (t1 r d e))
            xgtd = ins e (\k -> keep (t2 a b c d k)) (\p q r -> push (t1 a b c) d (t1 p q r))
            xeqb = keep (t2 a x c d e)
            xeqd = keep (t2 a b c x e)

        i (T1 a b c) keep push = select1 x b xltb xeqb xgtb
          where
            xltb = ins a (\k -> keep (t1 k b c)) (\p q r -> keep (t2 p q r b c))
            xgtb = ins c (\k -> keep (t1 a b k)) (\p q r -> keep (t2 a b p q r))
            xeqb = keep (t1 a x c)

delete :: forall a. Ord a => a -> Tree a -> Tree a
delete x (Tree tree) = undefined
