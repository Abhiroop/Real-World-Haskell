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

-- class Generic a where

--   type Rep a :: * -> *

--   from :: a -> (Rep a) p

--   to :: (Rep a) p -> a

data D1_Tree
data C1_Tree_Leaf
data C1_Tree_Branch

instance Generic (Tree a) where

  type Rep (Tree a) =
        M1 D D1_Tree (
            M1 C C1_Tree_Leaf U1
            :+:
            M1 C C1_Tree_Branch (
                M1 S NoSelector (K1 R (Tree a))
                :*:
                M1 S NoSelector (K1 R a)
                :*:
                M1 S NoSelector (K1 R (Tree a))
            )
        )

  from Leaf                     = M1 (L1 (M1 U1))
  from (Branch left root right) = M1 (
                                        R1 (
                                        M1 (
                                            M1 (K1 left)
                                            :*:
                                            M1 (K1 root)
                                            :*:
                                            M1 (K1 right)
                                        ))
                                    )

  to (M1 (L1 (M1 U1)))      = Leaf
  to (M1 (
            R1 (
            M1 (
                M1 (K1 left)
                :*:
                M1 (K1 root)
                :*:
                M1 (K1 right)
            ))
        ))                    = Branch left root right

instance Datatype D1_Tree where

  datatypeName _ = "Tree"

  moduleName _ = "Main"

instance Constructor C1_Tree_Leaf where

  conName _ = "Leaf"

instance Constructor C1_Tree_Branch where

  conName _ = "Branch"


data Tree2 a = Leaf2 | Branch2 (Tree2 a) a (Tree2 a) deriving (Show,Generic)


class Serializable' f where

  put' :: f p -> [Bit]
  get' :: [Bit] -> (f p, [Bit])

{-
data V1 p

infixr 5 :+:
data (:+:) f g p = L1 (f p) | R1 (g p)

data U1 p = U1

infixr 6 :*:
data (:*:) f g p = f p :*: g p

newtype K1 i a p = K1 { unK1 :: a }

newtype M1 i c f p = M1 { unM1 :: f p }
-}
