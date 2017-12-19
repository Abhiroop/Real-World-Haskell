{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module AssociatedTypes where

import Control.Concurrent.MVar
import Data.IORef

class IOStore store where
  newIO :: a -> IO (store a)
  getIO :: store a -> IO a
  putIO :: store a -> a -> IO ()

instance IOStore MVar where
  newIO = newMVar
  getIO = readMVar
  putIO mvar a = modifyMVar_ mvar (return . const a)

instance IOStore IORef where
  newIO = newIORef
  getIO = readIORef
  putIO ioref a = modifyIORef ioref (const a)
