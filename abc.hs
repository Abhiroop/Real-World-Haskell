import Control.Concurrent.STM

action :: STM Integer
action = do
  x <- newTVar 50
  y <- readTVar x
  return y

main :: IO ()
main = do
  z <- atomically action
  print z
