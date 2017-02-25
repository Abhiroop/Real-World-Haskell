module ReaderPractice where

import Control.Applicative
import Data.Maybe

x=[1,2,3]
y=[4,5,6]
z=[7,8,9]

--lookup :: Eq a => a -> [(a,b)] -> Maybe b

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = do
  x <- xs
  y <- ys
  return (x,y)
-- x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = do
  y <- ys
  z <- zs
  return (y,z)
-- x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ foldr (&&) True $ sequA 7
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
  print $ bolt $ fromMaybe 0 (z' 3)