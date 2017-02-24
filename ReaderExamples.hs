{-# LANGUAGE InstanceSigs #-}
module ReaderExmaples where

import Control.Applicative

import Data.Char

hurr = (*2)

durr = (+10)

m :: Integer -> Integer
m = hurr.durr

m' :: Integer -> Integer
m' = fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

hurrDurr :: Integer -> Integer
hurrDurr = do
  a <- hurr
  b <- durr
  return (a + b)

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = reverse.cap

fmapped :: [Char] -> [Char]
fmapped = fmap reverse cap

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char],[Char])
tupled' = do
  a <- cap
  b <- rev
  return (a,b)

newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader a a
ask =  Reader id

newtype HumanName = HumanName String deriving (Eq,Show)
newtype DogName = DogName String deriving (Eq,Show)
newtype Address = Address String deriving (Eq,Show)

data Person =
  Person {
    humanName :: HumanName,
    dogName :: DogName,
    address :: Address
         }deriving (Eq,Show)

data Dog =
  Dog {
    dogsname :: DogName,
    dogsAddress :: Address
      } deriving (Eq,Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =  liftA2 Dog dogName address

asks :: (r->a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap :: (a->b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f.ra)

instance Applicative (Reader r) where

  pure :: a -> Reader r a
  pure a = Reader $ undefined

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> undefined
