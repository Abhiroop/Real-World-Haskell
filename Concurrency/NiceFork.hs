module NiceFork(ThreadManager, newManager --forkManaged, getStatus, waitFor, waitAll
               ) where

import Control.Concurrent
import Control.Exception (Exception, try)
import qualified Data.Map as M

data ThreadException = KilledByUncaughtException deriving(Eq, Show)

instance Exception ThreadException

data ThreadStatus = Running | Finished | Threw ThreadException deriving (Eq,Show)

newtype ThreadManager = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus))) deriving Eq

-- creates a new Thread Manager
newManager :: IO ThreadManager
newManager = do
  mvar <- newMVar M.empty
  return $ Mgr mvar

-- creates a new managed thread
forkManged :: ThreadManager -> IO () -> IO ThreadId
forkManged (Mgr mgr) body =
--modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
  modifyMVar mgr $ \m -> do
    state <- newEmptyMVar
    tid <- forkIO $ do
      result <- try body
      putMVar state (either Threw (const Finished) result)
    return (M.insert tid state m, tid)

-- immediately return the status of the thread
--getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

-- -- block until a specific manged thread terminates
-- waitFor :: Threadmanager -> ThreadId -> IO (Maybe ThreadStatus)

-- -- block until all manged threads terminate
-- waitAll :: ThreadManger -> IO()

