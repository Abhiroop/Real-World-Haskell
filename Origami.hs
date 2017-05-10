{- LANGUAGE OverloadedStrings -}
module Origami where
import Text.Regex.Posix.Wrap
import Data.Function(fix)
import qualified Data.Map.Strict as M hiding (map)
import Data.Maybe (fromJust)
import Data.Graph
import System.IO.Unsafe
import Foreign.C.String
import Data.Either
import Control.Applicative
data Fix s a = FixT {getFix :: s a (Fix s a)}

data List_ a r = Nil | Cons_ a r
  deriving (Show)

data Tree_ a r = Leaf_ a | Node_ a r r
  deriving (Show)

type ListF a = Fix List_ a
type TreeF a = Fix Tree_ a

data Info = Info {name :: String,parent :: [String]}


type ClassName = String
type Parent = String

parseClassHeader :: ClassName -> [Parent]
parseClassHeader c = M.findWithDefault [] c $ M.fromList [("D",["B"]),("B",["A"]),("E",["B"]),("F",["C"]),("G",["C"]),("C",["A"])]

fixPointFunc :: [(ClassName,[Parent])] -> ClassName -> [(ClassName,[Parent])]
fixPointFunc x c =
  let parent = parseClassHeader c
   in if null parent
         then x
         else let j = x ++ [(c,parent)]
               in concatMap (fixPointFunc j) parent

fixPointFunc' :: [(ClassName,[Parent])] -> ClassName -> [(ClassName,[Parent])]
fixPointFunc' =
  fix $ \ recur x c ->  let parent = parseClassHeader c
                         in if null parent
                             then x
                             else let j = x ++ [(c,parent)]
                                   in concatMap (recur j) parent

loeb :: (Functor f) => f (f a -> a) -> f a
loeb x = fmap (\ a -> a (loeb x)) x
-- loeb x = go where go = fmap ($ go) x



factorial n = fac n 1

fac 1 acc = acc
fac n acc = fac (n-1) (acc*n)


f :: (IO a, IO a) -> IO (a, a)
f (x,y)= do
  a <- x
  b <- y
  return (a,b)

f' :: (IO a, IO a) -> IO (a,a)
f' = uncurry $ liftA2 (,)


g :: IO ()
g = do
  x <- getLine
  case (length x) of
    0 -> putStrLn "Hello"
    _ -> putStrLn "World"

