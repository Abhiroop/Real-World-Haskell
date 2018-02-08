module LeafNumbering where

import Prelude hiding(traverse)

-- Give a Tree number the leaves in increasing order.

data Tree = Leaf Int | Node (Int,Int) Tree Tree deriving Show

leaves :: Tree -> Int
leaves (Leaf l) = l
leaves (Node (x,y) _ _) = x + y

traverse :: Tree -> Tree
traverse (Leaf l) = (Leaf l)
traverse (Node n x y) = Node ((leaves x),(leaves y)) (traverse x) (traverse y)
