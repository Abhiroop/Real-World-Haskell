{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs#-}
module HList where

data HList tys where
  Nil :: HList '[]
  (:>) :: h -> HList t -> HList (h ': t)
infixr 5 :>
