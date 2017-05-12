module Echo2 where

import Control.Monad (replicateM_, forever)
import Pipes
import Prelude hiding (take, head)
import qualified Pipes.Prelude as P

main :: IO ()
main = runEffect $ P.stdinLn >-> P.stdoutLn

take ::  Int -> Pipe a a IO ()
take n = do
    replicateM_ n $ do                     -- Repeat this block 'n' times
        x <- await                         -- 'await' a value of type 'a'
        yield x                            -- 'yield' a value of type 'a'
    lift $ putStrLn "You shall not pass!"  -- Fly, you fools!

abc = runEffect $ P.stdinLn >-> take 3 >-> P.stdoutLn

cat :: Monad m => Pipe a a m r
cat = forever $ do
    x <- await
    yield x

head :: Monad m => Int -> Pipe a a m ()
head = P.take

yes :: Monad m => Producer String m r
yes = forever $ yield "y"

test :: IO ()
test = runEffect $ yes >-> head 3 >-> P.stdoutLn
