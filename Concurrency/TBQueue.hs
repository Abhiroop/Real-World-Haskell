module TBQueue where

import Control.Concurrent.STM hiding (TBQueue,
                                     newTBQueue,
                                     writeTBQueue,
                                     readTBQueue)

data TBQueue a = TBQueue (TVar Int) (TVar [a]) (TVar [a])

newTBQueue :: Int -> STM (TBQueue a)
newTBQueue size = do
  read <- newTVar []
  write <- newTVar []
  cap <- newTVar size
  return (TBQueue cap read write)

writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue (TBQueue cap _ write) a = do
  avail <- readTVar cap
  if avail ==0
    then retry --block here
    else writeTVar cap (avail - 1)
  listend <- readTVar write
  writeTVar write (a:listend)

readTBQueue :: TBQueue a -> STM a
readTBQueue (TBQueue cap read write) = do
  avail <- readTVar cap
  writeTVar cap (avail + 1)
  xs <- readTVar read
  case xs of
    (x:xs') -> do
      writeTVar read xs'
      return x
    [] -> do
      ys <- readTVar write
      case ys of
        [] -> retry
        _ -> do
          let (z:zs) = reverse ys -- This is lazy
          writeTVar write []
          writeTVar read zs
          return z

-- Try implementing peek

-- newTBQueue :: Int -> STM (TBQueue a)
-- writeTBQueue :: TBQueue a -> a -> STM ()
-- readTBQueue :: TBQueue a -> STM a
action :: STM ()
action = do
  x <- newTBQueue 6
  writeTBQueue x "hello"
  -- readTBQueue x

main :: IO ()
main = atomically action
