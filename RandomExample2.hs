{-# LANGUAGE InstanceSigs #-}
module RandomExample2 where

import Control.Applicative (liftA2,liftA3)
import Control.Monad (liftM,replicateM)
import Control.Monad.Trans.State
import System.Random

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq,Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)


rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

------------------------------------------------------

newtype Moi s a = Moi { runMoi :: s -> (a,s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a,_) = g s
                               in  (f a, s)

instance Applicative (Moi s) where
  pure :: a->Moi s a
  pure a = Moi $ \s -> (a,s)

  (<*>) :: Moi s (a->b) ->Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (ab,_) = f s
                                        (a,_)  = g s
                                    in (ab a,s)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (a,_) = f s
                                  sbs   = runMoi (g a)
                              in  sbs s






































