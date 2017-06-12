{-# LANGUAGE DefaultSignatures,DeriveGeneric,FlexibleContexts,TypeFamilies,TypeOperators #-}

module Generics where

import GHC.Generics

data Bit = O | I deriving (Eq, Show)




