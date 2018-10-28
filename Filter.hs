{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

import Prelude hiding (filter, foldr, foldr1)
import GHC.Types ( SPEC(..) )

data Step s a where
  Yield :: a -> s -> Step s a
  Skip  :: s -> Step s a
  Done  :: Step s a

-- This resembles vector
data Stream m a = forall s. Stream (s -> m (Step s a)) s

{-# INLINE toStream #-}
toStream :: Monad m => [a] -> Stream m a
toStream = Stream step
  where
    step (x:xs) = return $ Yield x xs
    step []     = return Done

{-# INLINE fromStream #-}
fromStream :: Monad m => Stream m a -> m [a]
fromStream = foldr (:) []

{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> Stream m a -> m b
foldr f = foldrM (\a b -> return (f a b))


{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> b -> m b) -> b -> Stream m a -> m b
foldrM f z (Stream step t) = foldrM_loop SPEC t
  where
    foldrM_loop !_ s
      = do
          r <- step s
          case r of
            Yield x s' -> f x =<< foldrM_loop SPEC s'
            Skip    s' -> foldrM_loop SPEC s'
            Done       -> return z

{-# INLINE filterM #-}
filterM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
filterM f (Stream step t) = Stream step' t
  where
    step' s = do
                r <- step s
                case r of
                  Yield x s' -> do
                                  b <- f x
                                  return $ if b then Yield x s'
                                                else Skip    s'
                  Skip    s' -> return $ Skip s'
                  Done       -> return $ Done

{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter f = filterM (return . f)


---------------------------------------------------------------------
data Step1 s a where
  Yield1 :: a -> s -> Step1 s a
  Done1  :: Step1 s a

-- This resembles streamly now
data Stream1 m a = forall s. Stream1 (s -> m (Step1 s a)) s

{-# INLINE foldr1 #-}
foldr1 :: Monad m => (a -> b -> b) -> b -> Stream1 m a -> m b
foldr1 f = foldrM1 (\a b -> return (f a b))

{-# INLINE foldrM1 #-}
foldrM1 :: Monad m => (a -> b -> m b) -> b -> Stream1 m a -> m b
foldrM1 f z (Stream1 step t) = foldrM_loop SPEC t
  where
    foldrM_loop !_ s
      = do
          r <- step s
          case r of
            Yield1 x s' -> f x =<< foldrM_loop SPEC s'
            Done1       -> return z

{-# INLINE filterM' #-}
filterM' :: Monad m => (a -> m Bool) -> Stream1 m a -> Stream1 m a
filterM' f (Stream1 step t) = Stream1 step' t
  where
    step' s = do
                r <- step s
                case r of
                  Yield1 x s' -> do
                                  b <- f x
                                  if b then return $ Yield1 x s'
                                       else step' s'
                  Done1       -> return $ Done1


{-# INLINE filter1 #-}
filter1 :: Monad m => (a -> Bool) -> Stream1 m a -> Stream1 m a
filter1 f = filterM' (return . f)

{-# INLINE toStream1 #-}
toStream1 :: Monad m => [a] -> Stream1 m a
toStream1 = Stream1 step
  where
    step (x:xs) = return $ Yield1 x xs
    step []     = return Done1

{-# INLINE fromStream1 #-}
fromStream1 :: Monad m => Stream1 m a -> m [a]
fromStream1 = foldr1 (:) []

-----------------------------------------------------------------
foo :: Monad m => Stream m Int -> Stream m Int
foo = filter (>0)

bar :: Monad m => Stream1 m Int -> Stream1 m Int
bar = filter1 (>0)

main :: IO ()
main = do
  let x = [1..10000000]
  m <- fromStream $ foo $ toStream x
  -- m <- fromStream1 $ bar $ toStream1 x
  print $ length m
