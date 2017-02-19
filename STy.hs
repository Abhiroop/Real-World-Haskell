{-# LANGUAGE GADTs#-}

module STy where

data STy ty where
  SInt :: STy Int
  SBool :: STy Bool
  SMaybe :: STy ty' -> STy (Maybe ty') --ty' is a phantom variable

zero :: STy ty -> ty
zero SInt = 0
zero SBool = False
zero (SMaybe _) = Nothing
