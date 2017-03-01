module TBQueue where

import Control.Concurrent.STM hiding (TBQueue)

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
