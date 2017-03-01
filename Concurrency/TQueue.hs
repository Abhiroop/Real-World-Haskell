module TQueue where

import Control.Concurrent.STM hiding (TQueue)

data TQueue a = TQueue (TVar [a]) (TVar [a])

newTQueue :: STM (TQueue a)
newTQueue = do
  read <- newTVar []
  write <- newTVar []
  return (TQueue read write)

writeTQueue :: TQueue a -> a -> STM ()
writeTQueue (TQueue _ write) a = do
  listend <- readTVar write
  writeTVar write (a:listend)

readTQueue :: TQueue a -> STM a
readTQueue (TQueue read write) = do
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
          let (z:zs) = reverse ys
          writeTVar write []
          writeTVar read zs
          return z
