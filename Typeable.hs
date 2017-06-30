{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Typeable where

import Data.Data
import Data.Typeable

data X = X deriving (Data, Typeable)

char :: Typeable a => a -> String
char x = case cast x of
           Just (x :: Char) -> show x
           Nothing -> "unknown"
