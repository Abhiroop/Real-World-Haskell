--A monad that supplies unique values of any kind

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply (Supply, next, runSupply)where

import Control.Monad.State

newtype Supply s a = S (State [s] a)
  deriving (Monad)

instance Functor (Supply s) where
  fmap = liftM

instance Applicative (Supply s) where
    pure = return
    (<*>) = ap


runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)
