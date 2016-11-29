module Dark where

import System.IO
import Data.Foldable

myFunc :: IO ()
myFunc = do
  withFile "/tmp/output.txt" WriteMode writeOutput
  where
    writeOutput handle = forM_ (replicate 10000000 "I love strict IO") (hPutStr handle)
