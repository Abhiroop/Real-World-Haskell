module Origami where

data Fix s a = FixT {getFix :: s a (Fix s a)}

data List_ a r = Nil | Cons_ a r
  deriving (Show)

data Tree_ a r = Leaf_ a | Node_ a r r
  deriving (Show)

type ListF a = Fix List_ a
type TreeF a = Fix Tree_ a
