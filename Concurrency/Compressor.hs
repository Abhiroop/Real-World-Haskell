module Compresoor where

import Control.Concurrent(forkIO)
import Control.Exception (handle)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L
import System.Console.Readline (readline)

import Codec.Compression.GZip (compress)

main = do
  maybeLine <- readline "Enter a file to compress"
  case maybeLine of
    Nothing -> return () -- user enterd EOF
    Just "" -> return () -- treat no name as "want to quit"
    Just name -> do
      handle ((\e -> print e) :: IOError -> IO ()) $ do
        content <- L.readFile name
        forkIO (compressFile name content)
        return ()
      main
 where compressFile path = L.writeFile (path ++ ".gz") . compress
