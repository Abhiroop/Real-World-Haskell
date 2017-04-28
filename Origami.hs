{- LANGUAGE OverloadedStrings -}
module Origami where

import Data.Function(fix)
import qualified Data.Map.Strict as M hiding (map)
import Data.Maybe (fromJust)
import Data.Graph
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


