module Parse where

import qualified Data.ByteString.Lazy as L
import Data.Int

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64          
    } deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
