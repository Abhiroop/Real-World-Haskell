module List where

import Pipes
import qualified Pipes.Prelude as P

pair :: ListT IO (Int, Int)
pair = do
    x <- Select $ each [1, 2]
    lift $ putStrLn $ "x = " ++ show x
    y <- Select $ each [3, 4]
    lift $ putStrLn $ "y = " ++ show y
    return (x, y)

--  every :: Monad m => ListT m a -> Producer a m ()

test  = runEffect $ (every pair) >->P.print
test2 = runEffect $ (every pair) >-> P.take 2 >->P.print
