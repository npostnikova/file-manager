module FileSystem.File
  ( File(..)
  , fileAppend
  , fileInfo
  , fileWrite
  , makeEmptyFile
  , makeFileWithContent
  , stringContent
  , toFileContent
  ) where

import qualified Data.ByteString      as S
import qualified Data.ByteString.UTF8 as BSU

import           Data.Time.Clock      (UTCTime)
import           System.Directory     (Permissions (..), emptyPermissions,
                                       setOwnerReadable, setOwnerWritable)

data File = File
  { fileName             :: String
  , filePermissions      :: Permissions
  , fileModificationTime :: UTCTime
  , fileSize             :: Int
  , fileContent          :: S.ByteString
  } deriving (Show)


showPermissions :: Permissions -> String
showPermissions perms = [r, w, e]
  where
    r = if readable   perms then 'r' else '-'
    w = if writable   perms then 'w' else '-'
    e = if executable perms then 'e' else '-'

fileInfo :: File -> [String]
fileInfo file =
  [ "Name:\t\t" ++ fileName file
  , "Size:\t\t" ++ show (fileSize file)
  , "Permissions:\t" ++ showPermissions (filePermissions file)
  , "Last modifies:\t" ++ show (fileModificationTime file)
  ]

makePermission :: Permissions
makePermission = setOwnerWritable True $ setOwnerReadable True emptyPermissions

makeEmptyFile :: UTCTime -> String -> File
makeEmptyFile time name = File
  { fileName             = name
  , filePermissions      = makePermission
  , fileModificationTime = time
  , fileSize             = 0
  , fileContent          = S.empty
  }

makeFileWithContent :: UTCTime -> String -> S.ByteString -> File
makeFileWithContent time name content = File
  { fileName             = name
  , filePermissions      = makePermission
  , fileModificationTime = time
  , fileSize             = S.length content
  , fileContent          = content
  }

fileAppend :: UTCTime -> String -> File -> File
fileAppend time str file = file
  { fileSize      = fileSize file + S.length content
  , fileContent   = fileContent file `S.append` content
  , fileModificationTime = time
  }
  where content = BSU.fromString str

fileWrite :: UTCTime -> String -> File -> File
fileWrite time str file = file
  { fileSize      = S.length content
  , fileContent   = content
  , fileModificationTime = time
  }
  where content = BSU.fromString str

toFileContent :: String -> S.ByteString
toFileContent = BSU.fromString

stringContent :: File -> String
stringContent = BSU.toString . fileContent