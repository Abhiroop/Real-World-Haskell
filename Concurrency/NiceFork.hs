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
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) threadId = do
  modifyMVar mgr $ \m ->
    case M.lookup threadId m of
      Nothing -> return (m,Nothing)
      Just mvar -> do
        status <- tryTakeMVar mvar
        case status of
          Nothing -> return (m, Just Running)
          Just sth -> return (M.delete threadId m, Just sth)
  {-
  map <- readMVar mgr
  case M.lookup threadId map of
    Just mvar -> do
      threadStatus <- readMVar mvar
      return (Just threadStatus)
    Nothing -> return Nothing-}

-- block until a specific managed thread terminates
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = do
  maybeDone <- modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _)  -> (m, Nothing)
      (Just st, m') -> (m', Just st)
  case maybeDone of
    Nothing -> return Nothing
    Just st -> Just `fmap` takeMVar st
    
-- -- block until all manged threads terminate
-- waitAll :: ThreadManger -> IO()

