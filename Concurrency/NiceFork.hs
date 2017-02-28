module NiceFork(ThreadManager, newManager, forkManaged, getStatus, waitFor, waitAll) where

import Control.Concurrent
import Control.Exception (Exception, try)
import qualified Data.Map as M

data ThreadException = KilledByUncaughtException deriving(Eq, Show)

instance Exception ThreadException

data ThreadStatus = Running | Finished | Threw ThreadException deriving (Eq,Show)

-- creates a new Thread Manager
newManager :: IO ThreadManager

-- creates a new managed thread
forkManged :: ThreadManger -> IO () -> IO ThreadId

-- immediately return the status of the thread
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

-- block until a specific manged thread terminates
waitFor :: Threadmanager -> ThreadId -> IO (Maybe ThreadStatus)

-- block until all manged threads terminate
waitAll :: ThreadManger -> IO()




