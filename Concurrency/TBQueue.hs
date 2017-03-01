module TBQueue where

import Control.Concurrent.STM hiding (TBQueue)

data TBQueue a = TBQueue (TVar Int) (TVar [a]) (TVar [a])

newTBQueue :: Int -> STM (TBQueue a)
newTBQueue size = do
  read <- newTVar []
  write <- newTVar []
  cap <- newTVar size
  return (TBQueue cap read write)
