{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Graph where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List


type Vertex = Int

type Graph = Map.Map Vertex [Vertex]


bfs :: Graph -> [Vertex]
bfs g = let ((vert,_):_) = Map.toList g
         in go [vert] []
  where
    go [] visited         = reverse visited
    go (x:xs) visited = let vs = fromJust $ g Map.!? x
                            actualvs = vs \\ visited
                         in if x `elem` visited
                               then go (xs ++ actualvs) visited
                               else go (xs ++ actualvs) (x : visited)

-- Optimizations
-- \\ operator is costly
-- elem is costly
-- reverse is costly
-- ++ in the end is costly
test =
  let g = Map.fromList [(1,[2,3]),
                        (2,[1,4,5]),
                        (3, [1,5,6]),
                        (4,[2,6]),
                        (5,[3,2,6]),
                        (6,[3,4,5])]
       in bfs g == [1,2,3,4,5,6]




data Tree a = Empty
            | Node a (Tree a) (Tree a)

traverseBF :: Tree a -> [a]
traverseBF tree = tbf [tree]
    where
        tbf [] = []
        tbf xs = map nodeValue xs ++ tbf (concatMap leftAndRightNodes xs)

        nodeValue (Node a _ _) = a

        leftAndRightNodes (Node _ Empty Empty) = []
        leftAndRightNodes (Node _ Empty b)     = [b]
        leftAndRightNodes (Node _ a Empty)     = [a]
        leftAndRightNodes (Node _ a b)         = [a,b]


createTree = Node 'A'
                (Node 'B'
                    (Node 'C' Empty Empty)
                    (Node 'D' Empty Empty)
                )
                (Node 'E'
                    (Node 'F' Empty Empty)
                    (Node 'G' Empty (Node 'H'
                        (Node 'I' Empty Empty)
                        Empty
                    ))
                )

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first, rest) = splitAt n list


data Z = Z -- rank-0
data tail :. head = tail :. head --increase rank by 1

type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int
type DIM3 = DIM2 :. Int

type family Dim a :: *
type instance Dim Z = ()
type instance Dim (t :. h) = (Dim t, Dim h)

data Array dim e where
  Array :: Dim dim -> e -> Array dim e

type VecList   = Array DIM1
type VecMatrix = Array DIM2


stencilM :: (VecMatrix a -> b) -> VecMatrix a -> VecMatrix b
stencilM = undefined

stencilL :: (VecList a -> b) -> VecList a -> VecList b
stencilL = undefined

class Comonad w where
  cobind :: (w a -> b) -> w a -> w b

instance Comonad VecMatrix where
  cobind = stencilM

instance Comonad VecList where
  cobind = stencilL
