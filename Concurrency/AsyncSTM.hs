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

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var

waitSTM :: Async a -> STM a
waitSTM a = do
  r <- waitCatchSTM a
  case r of
    Left e -> throwSTM e
    Right a -> return a

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $
  fmap Left (waitSTM a)
    `orElse`
  fmap Right (waitSTM b)

waitAny :: [Async a] -> IO a
waitAny asyncs = atomically $ foldr orElse retry $ map waitSTM asyncs
