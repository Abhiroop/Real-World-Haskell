module Echo where

import Control.Monad (unless)
import Control.Exception (try, throwIO)
import Pipes
import System.IO (isEOF)
import qualified GHC.IO.Exception as G

stdinLn :: Producer String IO ()
stdinLn = do
  eof <- lift isEOF
  unless eof $ do
    str <- lift getLine
    yield str
    stdinLn

loop :: Effect IO ()
loop = for stdinLn (lift . putStrLn)

triple :: Monad m => a -> Producer a m ()
triple x = do
    yield x
    yield x
    yield x


main :: IO ()
main = runEffect loop

stdoutLn :: Consumer String IO ()
stdoutLn = do
    str <- await  -- 'await' a 'String'
    x   <- lift $ try $ putStrLn str
    case x of
        -- Gracefully terminate if we got a broken pipe error
        Left e@(G.IOError { G.ioe_type = t}) ->
            lift $ unless (t == G.ResourceVanished) $ throwIO e
        -- Otherwise loop
        Right () -> stdoutLn

doubleUp :: Monad m => Consumer String m String
doubleUp = do
    str1 <- await
    str2 <- await
    return (str1 ++ str2)

-- yield :: Monad m => a -> Producer a m ()
-- (~>) :: Monad m => (a -> Producer b m ()) -> (b -> Producer c m ()) -> (a -> Producer c m ())



-- await :: Monad m => Consumer a m a
-- (>~) :: Monad m => Effect m b -> Consumer b m c -> Effect m c



-- (>->) :: Monad m => Producer a m r -> Consumer a m r -> Effect m r









