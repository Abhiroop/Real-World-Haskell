{-# LANGUAGE OverloadedStrings #-}
module Scotty2 where

import Web.Scotty

import Control.Monad.IO.Class
import Data.Monoid (mconcat)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    liftIO (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

