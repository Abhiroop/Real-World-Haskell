module TreeMap where

data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)

treeLengths :: Foldable t => Tree (t a) -> Tree Int
treeLengths (Leaf s) = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

instance Functor Tree where
    fmap = treeMap

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a)   = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)
