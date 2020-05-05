module FileManager.Action
  ( append
  , deleteFile
  , mkdir
  , mkdirsIfAbsent
  , mkfile
  , rewrite
  ) where

import System.Directory (removeFile, createDirectory, createDirectoryIfMissing)
import qualified Data.ByteString as S

mkdir :: FilePath -> IO ()
mkdir = createDirectory

mkdirsIfAbsent :: FilePath -> IO ()
mkdirsIfAbsent = createDirectoryIfMissing True

mkfile :: FilePath -> S.ByteString -> IO ()
mkfile = S.writeFile

rewrite :: FilePath ->  S.ByteString -> IO ()
rewrite = S.writeFile

append :: FilePath -> S.ByteString -> IO ()
append = S.appendFile

deleteFile :: FilePath -> IO ()
deleteFile = removeFile
