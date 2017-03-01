module TQueue where

import Control.Concurrent.STM

data TQueue a = TQueue (TVar [a]) (TVar [a])
