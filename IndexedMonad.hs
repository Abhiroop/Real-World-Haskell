{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module IndexedMonad where

data FHState where
  Open   :: FHState
  Closed :: FHState

data WitnessFHState (state :: FHState) where
  WitnessOpen :: WitnessFHState 'Open
  WitnessClosed :: WitnessFHState 'Closed


data Witness a (s1 :: state) (s2 :: state) where
  Witness :: a -> Witness a t t

type a :-> b = forall state. a state -> b state


