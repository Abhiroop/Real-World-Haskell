module BetterPredicate where

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock(UTCTime(..))
--import System.FilePath (takeExtension)
import Control.Exception (SomeException(..), handle, bracket)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(SomeException _) -> return Nothing) $ do
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)
    
betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms <- getPermissions name
          size <- getFileSize name
          modified <- getModificationTime name
          return (p name perms size modified)

