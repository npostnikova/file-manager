module FileSystem.Directory
  ( Directory(..)
  , Component(..)
  , FSItem(..)
  , DirectoryContent
  , dirContentDelete
  , dirContentInsert
  , dirLookup
  , getDirContent
  , getTree
  , innerDirs
  , innerFiles
  , listDirContent
  , makeEmptyDir
  , satisfyComponents
  ) where

import           Data.Either      (lefts, rights)
import           Data.Foldable    (foldMap)
import           Data.List        (intercalate)
import qualified Data.Map         as Map
import           Data.Time.Clock  (UTCTime)
import           FileSystem.File  (File (..), fileInfo)
import           System.Directory (Permissions (..), emptyPermissions,
                                   setOwnerReadable, setOwnerWritable)

data Component
  = CDir Directory
  | CFile File
  deriving (Show)

type DirectoryContent = Map.Map String Component

data Directory = Directory
  { dirName        :: String
  , dirPermissions :: Permissions
  , dirSize        :: Int
  , dirFilesNum    :: Int
  , dirContent     :: DirectoryContent
  } deriving (Show)

showPermissions :: Permissions -> String
showPermissions perms = [r, ' ', w, ' ', e]
  where
    r = if readable   perms then 'r' else '-'
    w = if writable   perms then 'w' else '-'
    e = if executable perms then 'e' else '-'

class FSItem a where
  getName     :: a -> String
  toComponent :: a -> Component
  getInfo     :: a -> [String]
  permissions :: a -> String
  isReadable  :: a -> Bool
  isWritable  :: a -> Bool


instance FSItem Component where
  getName (CFile f) = fileName f
  getName (CDir  d) = dirName  d

  toComponent = id

  getInfo (CFile f) = getInfo f
  getInfo (CDir  d) = getInfo d

  permissions (CFile f) = permissions f
  permissions (CDir d)  = permissions d

  isReadable (CFile f) = isReadable f
  isReadable (CDir  d) = isReadable d

  isWritable (CFile f) = isWritable f
  isWritable (CDir  d) = isWritable d

instance FSItem File where
  getName     = fileName
  toComponent = CFile
  getInfo     = fileInfo
  permissions = showPermissions . filePermissions
  isWritable  = writable . filePermissions
  isReadable  = readable . filePermissions

instance FSItem Directory where
  getName     = dirName
  toComponent = CDir
  getInfo     = dirInfo
  permissions = showPermissions . dirPermissions
  isWritable  = writable . dirPermissions
  isReadable  = readable . dirPermissions

dirElems :: Directory -> [Component]
dirElems dir = Map.elems $ dirContent dir

getTreeList :: Component -> [String]
getTreeList (CFile file) = [getName file]
getTreeList (CDir  dir ) = (getName dir):shiftedChildren
 where
  pref = replicate 5 ' '
  shiftedChildren = foldMap (formatter . getTreeList) (dirElems dir)
  formatter []     = []
  formatter (x:xs) = (" |___" ++ x):(map ((++) pref) xs)

getTree :: Directory -> String
getTree dir = intercalate "\n" $ getTreeList (CDir dir)

getDirContent :: Directory -> String
getDirContent dir = intercalate "\n" $ map getName (dirElems dir)

dirInfo :: Directory -> [String]
dirInfo dir =
  [ "Name:\t\t" ++ dirName dir
  , "Size:\t\t" ++ show (dirSize dir)
  , "Files inside:\t" ++ show (dirFilesNum dir)
  , "Permissions:\t" ++ permissions dir
  ]


getContentInfo :: Directory -> [String]
getContentInfo dir = foldMap mapper list
  where
   list = Map.elems (dirContent dir)
   mapper (CFile f) = fileInfo f
   mapper (CDir  d) = dirInfo  d

makePermission :: Permissions
makePermission = setOwnerWritable True $ setOwnerReadable True emptyPermissions

makeEmptyDir :: String -> Directory
makeEmptyDir name = Directory
  { dirName        = name
  , dirPermissions = makePermission
  , dirSize        = 0
  , dirFilesNum    = 0
  , dirContent     = Map.empty
  }

satisfyComponents :: (String -> Bool) -> Directory -> [String]
satisfyComponents keyPred dir = Map.keys $ Map.filterWithKey (\k _v -> keyPred k) (dirContent dir)

listDirContent :: Directory -> String
listDirContent dir = intercalate "\n" $ map getName (dirElems dir)

dirLookup :: String -> Directory -> Maybe Component
dirLookup key dir = Map.lookup key (dirContent dir)

dirContentInsert :: FSItem a => a -> DirectoryContent -> DirectoryContent
dirContentInsert item cont = Map.insert (getName item) (toComponent item) cont

dirContentDelete :: String -> DirectoryContent -> DirectoryContent
dirContentDelete name cont = Map.delete name cont

dirEitherContent :: Directory -> [Either File Directory]
dirEitherContent = map mapper . dirElems
  where
    mapper (CFile f) = Left f
    mapper (CDir  d) = Right d

innerDirs :: Directory -> [Directory]
innerDirs = rights . dirEitherContent

innerFiles :: Directory -> [File]
innerFiles = lefts . dirEitherContent

