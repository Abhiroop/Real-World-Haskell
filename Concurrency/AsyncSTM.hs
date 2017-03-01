module AsyncSTM where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception


data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  t   <- forkFinally action (atomically . putTMVar var)
  return (Async t var)
