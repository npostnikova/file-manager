module FileSystem.Reader
  ( readDirectory
  ) where

import           Control.Exception                 (Exception, catch, throwIO)
import           Control.Monad                     (filterM, (<=<), (>=>))
import qualified Data.ByteString.Char8             as S8
import           Data.Foldable                     (foldMap)
import           Data.List                         (last)
import qualified Data.Map                          as Map
import           Data.Monoid                       (Sum (..))
import           Data.Time.Clock                   (UTCTime, getCurrentTime)
import           FileSystem.Directory
import           FileSystem.File
import           NameManip                         (guessDotDot, slicePath)
import           System.Directory                  (Permissions,
                                                    createDirectoryIfMissing,
                                                    doesDirectoryExist,
                                                    doesFileExist,
                                                    emptyPermissions,
                                                    executable,
                                                    getModificationTime,
                                                    getPermissions,
                                                    listDirectory,
                                                    pathIsSymbolicLink,
                                                    readable, searchable,
                                                    writable)
import           System.Directory.Internal.Prelude (isPermissionError)
import           System.FilePath                   ((</>))
import           System.IO.Error                   (IOError)


-- | Permission error handler.
permissionHandler :: IO a     -- ^ Default value
                  -> IOError  -- ^ Error
                  -> IO a     -- ^ Result
permissionHandler def e = if isPermissionError e
                        then def
                        else throwIO e

permissionsGetter :: FilePath -> IO Permissions
permissionsGetter path = getPermissions path
                         `catch`
                         permissionHandler (return emptyPermissions)

modificationGetter :: FilePath -> IO UTCTime
modificationGetter path = getModificationTime path
                          `catch`
                          permissionHandler getCurrentTime

-- | Read information about file.
readFileInfo :: FilePath -- ^ Path to parent directory.
             -> String   -- ^ File name.
             -> IO File  -- ^ Result object.
readFileInfo pathToFile name = do
  let path = pathToFile </> name
  modificationTime <- modificationGetter path
  permissions      <- permissionsGetter  path
  content          <- S8.readFile        path
                      `catch`
                      permissionHandler (return S8.empty)
  return File
    { fileName             = name
    , filePermissions      = permissions
    , fileModificationTime = modificationTime
    , fileSize             = S8.length content
    , fileContent          = content
    }

-- | Returns empty directory if listDirectory fails because of permissions..
readDirectory :: FilePath -> IO Directory
readDirectory path = readDir path
                     `catch`
                     permissionHandler (return (makeEmptyDir name))
  where name = last ("":slicePath path)

readDir :: FilePath -> IO Directory
readDir path = do
  let name = last ("":slicePath path)
  modificationTime <- modificationGetter path
  permissions      <- permissionsGetter  path
  inside           <- listDirectory      path
  notSymbLinks     <- filterM symbFilter inside
  dirNames         <- filterM dirFilter  notSymbLinks
  fileNames        <- filterM fileFilter notSymbLinks

  files  <- sequenceA $ map (readFileInfo path)          fileNames
  dirs   <- sequenceA $ map (readDirectory . (</>) path) dirNames
  let cFiles = map (\f -> (fileName f, CFile f)) files
  let cDirs  = map (\d -> (dirName  d, CDir  d)) dirs
  return Directory
    { dirName        = name
    , dirPermissions = permissions
    , dirSize        = getSum $ foldMap (Sum . fileSize) files <> foldMap (Sum . dirSize) dirs
    , dirFilesNum    = length files + getSum (foldMap (Sum . dirFilesNum) dirs)
    , dirContent     = Map.fromList $ cFiles ++ cDirs
    }
  where
    notSymbLink = (return . not) <=< pathIsSymbolicLink
    symbFilter  = notSymbLink        . (</>) path
    dirFilter   = doesDirectoryExist . (</>) path
    fileFilter  = doesFileExist      . (</>) path

