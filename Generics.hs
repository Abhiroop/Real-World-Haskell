{-# LANGUAGE DefaultSignatures,DeriveGeneric,FlexibleContexts,TypeFamilies,TypeOperators #-}

module Generics where

import GHC.Generics

data Bit = O | I deriving (Eq, Show)

class Serializable a where
  put :: a -> [Bit]
  get :: [Bit] -> (a, [Bit])

data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving Show

instance Serializable a => Serializable (Tree a) where

  put Leaf = [O]
  put (Branch left root right) = [I] ++
                                 put left ++
                                 put root ++
                                 put right

  get (O:bits) = (Leaf,bits)
  get (I:bits) = (Branch left root right, bits''') where
    (left,bits') = get bits
    (root,bits'') = get bits'
    (right,bits''') = get bits''


