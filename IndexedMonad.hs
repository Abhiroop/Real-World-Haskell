{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module IndexedMonad where

import Prelude hiding (id, (.))
data FHState where
  Open   :: FHState
  Closed :: FHState

data WitnessFHState (state :: FHState) where
  WitnessOpen :: WitnessFHState 'Open
  WitnessClosed :: WitnessFHState 'Closed


data Witness a (s1 :: state) (s2 :: state) where
  Witness :: a -> Witness a t t

type a :-> b = forall state. a state -> b state

-- | Let us define identity and composition

id :: a :-> a
id x = x

(.) :: (b :-> c) -> (a :-> b) -> (a :-> c)
(.) f g = \x -> f (g x)

-- | We have identity and composition. So now we have a category. A "slice category".

class IFunctor f where
  imap :: (a :-> b) -> f a :-> f b

class IFunctor m => IMonad m where
  iskip   :: a :-> m a
  iextend :: (a :-> m b) -> m a :-> m b
