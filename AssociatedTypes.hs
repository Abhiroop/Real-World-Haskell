{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module AssociatedTypes where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.IORef
import Data.Map hiding (map)
-- Mimic Functional Dependencies
class Store store where
  type StoreMonad store :: * -> *
  new :: a -> (StoreMonad store) (store a)
  get :: store a -> (StoreMonad store) a
  put :: store a -> a -> (StoreMonad store) ()

instance Store MVar where
  type StoreMonad MVar = IO
  new = newMVar
  get = readMVar
  put mvar a = modifyMVar_ mvar (return . const a)

instance Store IORef where
  type StoreMonad IORef = IO
  new = newIORef
  get = readIORef
  put ioref a = modifyIORef ioref (const a)

instance Store TVar where
  type StoreMonad TVar = STM
  new = newTVar
  get = readTVar
  put ioref a = modifyTVar ioref (const a)

-- Implicit coercions
class Add a b where
  type SumTy a b
  add :: a -> b -> SumTy a b

instance Add Integer Double where
  type SumTy Integer Double = Double
  add x y = fromIntegral x + y

instance Add Double Integer where
  type SumTy Double Integer = Double
  add x y = x + fromIntegral y

instance Num a => Add a a where
  type SumTy a a = a
  add x y = x + y

-- Heterogenous Lists
class Cons a b where
  type ResTy a b
  cons :: a -> [b] -> [ResTy a b]

instance Cons Integer Double where
  type ResTy Integer Double = Double
  cons x ys = fromIntegral x : ys

-- data families
class Graph g where
  type Vertex g
  data Edge g
  src :: Edge g -> Vertex g
  tgt :: Edge g -> Vertex g
  outEdges :: g -> Vertex g -> [Edge g]

newtype G1 = G1 [Edge G1]

instance Graph G1 where
  type Vertex G1 = Int
  data Edge G1 = MkEdge1 (Vertex G1) (Vertex G1)
  src = undefined
  tgt = undefined
  outEdges = undefined

newtype G2 = G2 (Map (Vertex G2) [Vertex G2])

instance Graph G2 where
  type Vertex G2 = String
  data Edge G2 = MkEdge2 Int (Vertex G2) (Vertex G2)
  src = undefined
  tgt = undefined
  outEdges = undefined

-- Recursive type functions
instance (Add Integer a) => Add Integer [a] where
  type SumTy Integer [a] = [SumTy Integer a]
  add x y = map (add x) y

-- Memoization
class Memo a where
  data Table a :: * -> *
  toTable :: (a -> w) -> Table a w
  fromTable :: Table a w -> (a -> w)

instance Memo Bool where
  data Table Bool w = TBool w w
  toTable f = TBool (f True) (f False)
  fromTable (TBool x y) b = if b then x else y

f :: Bool -> Int
f = undefined

g :: Bool -> Int
g = fromTable (toTable f)


instance (Memo a, Memo b) => Memo (Either a b) where
  data Table (Either a b) w = TSum (Table a w) (Table b w)
  toTable f = TSum (toTable (f . Left)) (toTable (f . Right))
  fromTable (TSum t _) (Left v) = fromTable t v
  fromTable (TSum _ t) (Right v) = fromTable t v

instance (Memo a, Memo b) => Memo (a,b) where
  newtype Table (a,b) w = TProduct (Table a (Table b w))
  toTable f = TProduct (toTable (\x -> toTable (\y -> f (x,y))))
  fromTable (TProduct t) (x,y) = fromTable (fromTable t x) y




