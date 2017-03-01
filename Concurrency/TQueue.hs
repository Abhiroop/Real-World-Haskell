module TQueue where

import Control.Concurrent.STM hiding (TQueue)

data TQueue a = TQueue (TVar [a]) (TVar [a])

newTQueue :: STM (TQueue a)
newTQueue = do
  read <- newTVar []
  write <- newTVar []
  return (TQueue read write)
