module TChan where

import Control.Concurrent.STM hiding (TChan)

data TChan a = TChan (TVar (TVarList a)) (TVar (TVarList a))

type TVarList a = TVar (TList a)

data TList a = TNil | TCons a (TVarList a)

newTChan :: STM (TChan a)
newTChan = do
  hole <- newTVar TNil
  read <- newTVar hole
  write <- newTVar hole
  return (TChan read write)

readTChan :: TChan a -> STM a
readTChan (TChan readVar _) = do
  listHead <- readTVar readVar
  head <- readTVar listHead
  case head of
    TNil -> retry
    TCons val tail -> do
      writeTVar readVar tail
      return val

-- writeTChan :: TChan a -> a -> STM ()
