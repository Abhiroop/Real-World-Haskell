module CPS where

import qualified System.Exit as E

add :: Num a => a -> a -> (a -> r) -> r
add a b = \ k -> k (a + b)

exitSuccess :: a -> IO ()
exitSuccess _ = E.exitSuccess

exitFailure :: Int -> IO ()
exitFailure = E.exitWith . E.ExitFailure

mul :: Num a => a -> a -> (a -> r) -> r
mul a b = \ k -> k (a * b)

dvd :: Fractional a => a -> a -> (a -> r) -> r
dvd a b = \ k -> k (a / b)

prog_3 :: IO ()
prog_3 = add 2 3 (\ two_plus_three -> mul two_plus_three 5 exitFailure)

prog_6 :: Num a => (a -> r) -> r
prog_6 = \k ->
  add 2 3 (\five ->
    add 7 9 (\sixteen ->
      mul five sixteen (\eighty ->
         add eighty 5 k)))
