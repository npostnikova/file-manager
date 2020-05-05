{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module FileManager.Monad
  ( DirectoryState(..)
  , ExecResult
  , FileManagerMonad
  , MyState(..)
  , MergeWay(..)
  , UsrMergeChoice(..)

  , append
  , cat
  , cd
  , cvsAddFile
  , cvsDeleteVersion
  , cvsFileHistory
  , cvsFileVersion
  , cvsInit
  , cvsRemoveFile
  , cvsUpdateFile
  , getCurPath
  , merge
  , mergeInteractive
  , information
  , fileCurDelete
  , fileRewrite
  , findFileInSubdirs
  , ls
  , mkfile
  , mkdir
  , resolveMerge
  , root
  , tree
  ) where

import           FileManager.Error          (FileManagerError (..))
import           FileSystem.Directory       (Component (..), Directory (..),
                                             FSItem (..), dirContentDelete,
                                             dirContentInsert, dirLookup,
                                             getTree, innerDirs, innerFiles,
                                             listDirContent, makeEmptyDir,
                                             satisfyComponents)
import           FileSystem.File            (File (..), fileAppend, fileWrite,
                                             makeEmptyFile, makeFileWithContent,
                                             stringContent, toFileContent)
import           NameManip                  (guessDotDot, slicePath)

import qualified FileManager.Action         as Action

import           Control.Applicative        ((<|>))
import           Control.Monad              (forM, when)
import           Control.Monad.Error.Class  (catchError, throwError)
import           Control.Monad.State        (State (..))
import           Control.Monad.State.Class  (get, gets, modify)
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Monad.Trans.Reader (ReaderT (..), ask)
import           Data.Algorithm.Diff        (Diff (..), getGroupedDiff)
import           Data.ByteString            (ByteString, empty)
import           Data.Foldable              (fold)
import           Data.List                  (init, intercalate, isPrefixOf,
                                             last, stripPrefix, tail)
import           Data.List.Split            (splitOn)
import           Data.Maybe                 (catMaybes, fromJust, isJust)
import           Data.Time.Clock            (UTCTime)
import           System.FilePath            (joinPath, takeBaseName, (</>))
import           Text.Read                  (readMaybe)
import           Text.Regex                 (matchRegex, mkRegex)

import           Control.Monad.Trans.Except (ExceptT, catchE, runExceptT,
                                             throwE)


type FileManagerMonad a = ReaderT UTCTime (ExceptT FileManagerError (State MyState)) a

data DirectoryState = DirectoryState
  { dirPath            :: FilePath
  , relativeDirs :: [String]
  }

instance Show DirectoryState where
  show state = intercalate "\n"
    [ "Path:      " ++ dirPath          state
    , "Relative:  " ++ show (relativeDirs state)
    ]

data MyState = MyState
  { curState       :: DirectoryState
  , cvsStateMb     :: Maybe DirectoryState

  , rootDirectory  :: Directory
  , rootPath       :: FilePath
  , slicedRootPath :: [String]
  , actions        :: [IO ()]
  }

type ExecResult r = (Either FileManagerError r, MyState)

getCurPath :: MyState -> String
getCurPath = (</>) "/" . joinPath . relativeDirs . curState

instance Show MyState where
  show state = intercalate "\n"
    [ "~ Root:"
    , show (rootPath state)
    , show (slicedRootPath state)
    , show (rootDirectory state)
    , getTree (rootDirectory state)
    , "~ Cur:\n"  ++ show (curState  state)
    , "~ Cvs:\n"  ++ show (cvsStateMb state)
    ]

delta :: Num i => (a -> i) -> a -> a -> i
delta f a b = f a - f b

dirFindComponent :: String -> Directory -> FileManagerMonad Component
dirFindComponent name dir =
  case dirLookup name dir of
    Just x  -> return x
    Nothing -> throwError $ NotFound name

dirFindFile :: String -> Directory -> FileManagerMonad File
dirFindFile name dir = do
  component <- dirFindComponent name dir
  case component of
    CFile file -> return file
    CDir _     -> throwError $ FileExpected name

dirFindDirectory :: String -> Directory -> FileManagerMonad Directory
dirFindDirectory name dir = do
  component <- dirFindComponent name dir
  case component of
    CDir directory -> return directory
    CFile _        -> throwError $ DirExpected name

dirInsertFile :: File -> Directory -> FileManagerMonad Directory
dirInsertFile file dir = do
  (filesDelta, sizeDelta) <-
    case dirLookup (fileName file) dir of
      Just (CFile f) -> return (0, delta fileSize file f)
      Just (CDir _)  -> throwError FileManagerFailure
      Nothing        -> return (1, fileSize file)
  let content = dirContentInsert file (dirContent dir)
  return dir
    { dirSize     = dirSize dir     + sizeDelta
    , dirFilesNum = dirFilesNum dir + filesDelta
    , dirContent  = content
    }

dirDeleteFile :: String -> Directory -> FileManagerMonad Directory
dirDeleteFile name dir = do
  file <- dirFindFile name dir
  let content = dirContentDelete name (dirContent dir)
  return dir
    { dirSize     = dirSize dir - fileSize file
    , dirFilesNum = dirFilesNum dir - 1
    , dirContent  = content
    }

dirInsertDirectory :: Directory -> Directory -> FileManagerMonad Directory
dirInsertDirectory d dir = do
  (filesDelta, sizeDelta) <-
    case dirLookup (dirName d) dir of
      Just (CFile _)  -> throwError FileManagerFailure
      Just (CDir d')  -> return (delta dirFilesNum d d', delta dirSize d d')
      Nothing         -> return (dirFilesNum d, dirSize d)
  let content = dirContentInsert d (dirContent dir)
  return dir
    { dirSize     = dirSize dir     + sizeDelta
    , dirFilesNum = dirFilesNum dir + filesDelta
    , dirContent  = content
    }

setCurState :: DirectoryState -> FileManagerMonad ()
setCurState dirState = modify $ \st -> st { curState = dirState }

setCvsStateMb :: Maybe DirectoryState -> FileManagerMonad ()
setCvsStateMb dirStateMb = modify $ \st -> st { cvsStateMb = dirStateMb }


setCvsState :: DirectoryState -> FileManagerMonad ()
setCvsState dirState = modify $ \st -> st { cvsStateMb = Just dirState }


setRootDir :: Directory -> FileManagerMonad ()
setRootDir dir = modify $ \st -> st { rootDirectory = dir }

updateDirRec :: [String] -> Directory -> Directory -> FileManagerMonad Directory
updateDirRec [] updDir dir = return updDir
updateDirRec (x:xs) updDir dir = do
  updateDir `catchError` \_ -> throwError FileManagerFailure
  where
    updateDir = do
      xDir <- dirFindDirectory x dir
      upd  <- updateDirRec xs updDir xDir
      dirInsertDirectory upd dir

findInnerDir :: String -> Directory -> FileManagerMonad Directory
findInnerDir name dir = case dirLookup name dir of
  Nothing -> throwError $ DirNotFound name
  Just (CFile _) -> throwError $ DirExpected name
  Just (CDir nameDir) -> return nameDir

getInnerDir :: [String] -> Directory -> FileManagerMonad Directory
getInnerDir [] dir = return dir
getInnerDir (x:xs) dir = do
  xDir <- findInnerDir x dir
  getInnerDir xs xDir

cvsDirName :: String
cvsDirName = ".cvs"

findCvsDir :: FilePath -> [String] -> [String] -> Directory -> FileManagerMonad (Maybe DirectoryState)
findCvsDir pathPrefix relPrefix dirsLeft dir =
  searchCvsInDir `catchError` \_ -> noCvsInDir dirsLeft
  where
    searchCvsInDir = do
      cvsDir <- dirFindDirectory cvsDirName dir
      return $ Just $ DirectoryState
        { dirPath      = pathPrefix </> cvsDirName
        , relativeDirs = relPrefix ++ [cvsDirName] }
    noCvsInDir []     = return Nothing
    noCvsInDir (x:xs) = do
      xDir <- dirFindDirectory x dir
      catchError
        (dirFindDirectory x dir >>= findCvsDir (pathPrefix </> x) (relPrefix ++ [x]) xs)
        (\_ -> throwError FileManagerFailure)

findCvsOnPath :: [String] -> FileManagerMonad (Maybe DirectoryState)
findCvsOnPath dirs = do
  root    <- gets rootPath
  rootDir <- gets rootDirectory
  let pathPrefix = root
  let relPrefix  = []
  findCvsDir pathPrefix relPrefix dirs rootDir

getRelativeSlicedPath :: FilePath -> FileManagerMonad [String]
getRelativeSlicedPath path = do
  let sliced = slicePath path
  slicedRoot <- gets slicedRootPath
  case stripPrefix slicedRoot sliced of
    Just diff -> return diff
    Nothing   -> throwError InvalidPathArg

mkDirsForCvs :: [String] -> Directory -> FileManagerMonad Directory
mkDirsForCvs [] dir = return dir
mkDirsForCvs (x:xs) dir = case dirLookup x dir of
  Nothing -> do
    updDir <- mkDirsForCvs xs (makeEmptyDir x)
    dirInsertDirectory updDir dir
  Just (CFile _) -> throwError CvsFailure
  Just (CDir xDir) -> do
    updDir <- mkDirsForCvs xs xDir
    dirInsertDirectory updDir dir

getAbsolutePath :: FilePath -> FileManagerMonad FilePath
getAbsolutePath argPath = do
  pathCur  <- gets (dirPath . curState)
  pathRoot <- gets rootPath
  let pathToGuess = if isPrefixOf "/" argPath
                    then pathRoot </> tail argPath
                    else pathCur  </> argPath
  case guessDotDot pathToGuess of
    Just p  -> return p
    Nothing -> throwError InvalidPathArg

stripPrefixM :: [String] -> [String] -> FileManagerMonad [String]
stripPrefixM pref list = case stripPrefix pref list of
  Just  x -> return x
  Nothing -> throwError FileManagerFailure

getCvsState :: FileManagerMonad DirectoryState
getCvsState = do
  cvsMb <- gets cvsStateMb
  case cvsMb of
    Nothing  -> throwError CvsNotInitialized
    Just cvs -> return cvs

getCurDir :: FileManagerMonad Directory
getCurDir = do
  rootDir <- gets rootDirectory
  relCur  <- gets (relativeDirs . curState)
  getInnerDir relCur rootDir

getCvsDir :: FileManagerMonad Directory
getCvsDir = do
  rootDir  <- gets rootDirectory
  cvsState <- getCvsState
  getInnerDir (relativeDirs cvsState) rootDir

setCurDir :: Directory -> FileManagerMonad ()
setCurDir dir = do
  relPath <- gets (relativeDirs . curState)
  rootDir <- gets rootDirectory
  updRoot <- updateDirRec relPath dir rootDir
  setRootDir updRoot

setCvsDir :: Directory -> FileManagerMonad ()
setCvsDir dir = do
  cvsState <- getCvsState
  rootDir  <- gets rootDirectory
  updRoot  <- updateDirRec (relativeDirs cvsState) dir rootDir
  setRootDir updRoot

checkReadable :: FSItem i => i -> FileManagerMonad ()
checkReadable i
  | isReadable i = return ()
  | otherwise    = throwError $ NoReadPermission (getName i)

checkWritable :: FSItem i => i -> FileManagerMonad ()
checkWritable i
  | isWritable i = return ()
  | otherwise    = throwError $ NoWritePermission (getName i)


cd :: FilePath -> FileManagerMonad ()
cd argPath = do
  cur     <- gets curState
  rootDir <- gets rootDirectory
  path    <- getAbsolutePath argPath
  relPath <- getRelativeSlicedPath path
  curDir  <- getInnerDir relPath rootDir
  checkReadable curDir
  setCurState DirectoryState
    { dirPath    = path
    , relativeDirs = relPath }
  cvsMb <- findCvsOnPath relPath
  setCvsStateMb cvsMb
  when (isJust cvsMb) do
    { cvs         <- getCvsState
    ; rootDir     <- gets rootDirectory
    ; pathToBuild <- stripPrefixM (init (relativeDirs cvs)) relPath
    ; cvsDir      <- getCvsDir
    ; updCvs      <- mkDirsForCvs pathToBuild cvsDir
    ; updRoot     <- updateDirRec (relativeDirs cvs) updCvs rootDir
    ; let cvsPath = dirPath cvs </> joinPath pathToBuild
    ; addAction $ Action.mkdirsIfAbsent cvsPath
    ; setRootDir updRoot
    ; setCvsState DirectoryState
      { dirPath      = cvsPath
      , relativeDirs = relativeDirs cvs ++ pathToBuild }
    }

getRootInnerDir :: FilePath -> FileManagerMonad Directory
getRootInnerDir pathArg = do
  rootDir    <- gets rootDirectory
  path       <- getAbsolutePath pathArg
  relPath    <- getRelativeSlicedPath path
  getInnerDir relPath rootDir

tree :: String -> FileManagerMonad String
tree pathArg = do
  dir <- getRootInnerDir pathArg
  return $ getTree dir

ls :: String -> FileManagerMonad String
ls pathArg = do
  dir <- getRootInnerDir pathArg
  return $ listDirContent dir

information :: FilePath -> FileManagerMonad String
information pathArg = do
  rootDir    <- gets rootDirectory
  path       <- getAbsolutePath pathArg
  relPath    <- getRelativeSlicedPath path
  info <- case relPath of
    [] -> return (getInfo rootDir)
    l  -> do
      inner <- getInnerDir (init l) rootDir
      comp  <- dirFindComponent (last l) inner
      return (getInfo comp)
  return $ intercalate "\n" info

addAction :: IO () -> FileManagerMonad ()
addAction action = do
  acts <- gets actions
  modify (\st -> st { actions = acts ++ [action]})

mkdirCurAction :: String -> FileManagerMonad ()
mkdirCurAction name = do
  relPath <- gets (dirPath . curState)
  addAction $ Action.mkdir (relPath </> name)

mkdirCvsAction :: String -> FileManagerMonad ()
mkdirCvsAction name = do
  cvs <- getCvsState
  addAction $ Action.mkdir ((dirPath cvs) </> name)

mkfileCurAction :: String -> FileManagerMonad ()
mkfileCurAction name = do
  relPath <- gets (dirPath . curState)
  addAction $ Action.mkfile (relPath </> name) empty

mkfileCvsAction :: String -> ByteString -> FileManagerMonad ()
mkfileCvsAction name content = do
  cvs <- getCvsState
  addAction $ Action.mkfile ((dirPath cvs) </> name) content

mkdir :: String -> FileManagerMonad ()
mkdir name = do
  cur <- getCurDir
  checkWritable cur
  case dirLookup name cur of
    Nothing -> do
      let dir = makeEmptyDir name
      updCur <- dirInsertDirectory dir cur
      setCurDir updCur
    _       -> throwError $ EntityExist name
  mkdirCurAction name

createEmptyFile :: String ->  FileManagerMonad File
createEmptyFile name = do
  time <- ask
  return $ makeEmptyFile time name

mkfile :: String -> FileManagerMonad ()
mkfile name = do
  cur <- getCurDir
  case dirLookup name cur of
    Nothing -> do
      file   <- createEmptyFile name
      updCur <- dirInsertFile file cur
      setCurDir updCur
    _       -> throwError $ EntityExist name
  mkfileCurAction name

createFile :: String -> ByteString -> FileManagerMonad File
createFile name content = do
  time <- ask
  return $ makeFileWithContent time name content

makeCvsFile :: String -> ByteString -> FileManagerMonad ()
makeCvsFile name content = do
  cvs <- getCvsDir
  case dirLookup name cvs of
    Nothing -> do
      file   <- createFile name content
      updCvs <- dirInsertFile file cvs
      setCvsDir updCvs
    _       -> throwError $ EntityExist name
  mkfileCvsAction name content

cvsInit :: FileManagerMonad ()
cvsInit = do
  cvs <- gets cvsStateMb
  case cvs of
    Nothing -> do
      mkdir cvsDirName
      cur <- gets curState
      setCvsState DirectoryState
        { dirPath      = dirPath cur </> cvsDirName
        , relativeDirs = relativeDirs cur ++ [cvsDirName]
        }
    _       -> throwError CvsInitialized

nameInCvs :: String -> String
nameInCvs = map (\c -> if c == '.' then '_' else c)

nameWithMsg :: String -> String
nameWithMsg name = nameInCvs name ++ "_msg"

getNameWithVersion :: String -> Int -> String
getNameWithVersion name v = nameInCvs name ++ "_" ++ show v

cvsAddFile :: String -> FileManagerMonad ()
cvsAddFile name = do
  cvs <- getCvsState
  cur <- getCurDir
  file <- dirFindFile name cur
  checkReadable file
  let vName = getNameWithVersion name 0
  makeCvsFile (nameInCvs name) (toFileContent "0")
  makeCvsFile (nameWithMsg vName) (toFileContent "initial")
  makeCvsFile vName (fileContent file)

cvsFileHistory :: String -> FileManagerMonad String
cvsFileHistory name = do
  cvs  <- getCvsDir
  file <- catchError
    (dirFindFile (nameInCvs name) cvs)
    (\_ -> throwError $ FileNotInCvs name)
  let vers  = satisfyComponents keyPred cvs
  let vNums = map (last . init . splitOn "_") vers
  let vInt  = map (fromJust . readMaybe) vNums
  let zipped = zip (vInt :: [Int]) vers
  connected <- forM zipped $ \(i, fileName) -> do
    { file <- dirFindFile fileName cvs
    ; return $ show i ++ ".\t" ++ stringContent file }
  return $ intercalate "\n" (name:connected)
  where
    reg       = mkRegex $ "^" ++ (nameInCvs name) ++ "_[0-9]+_msg$"
    keyPred s = case matchRegex reg s of
      Just  _ -> True
      Nothing -> False

fileCvsRewrite :: String -> String -> FileManagerMonad ()
fileCvsRewrite name content = do
  time   <- ask
  cvs    <- getCvsDir
  file   <- dirFindFile name cvs
  checkWritable file
  checkReadable file
  let updFile = fileWrite time content file
  updCvs <- dirInsertFile updFile cvs
  setCvsDir updCvs
  cvs     <- getCvsState
  addAction $ Action.rewrite (dirPath cvs </> name) (fileContent updFile)

fileRewrite :: String -> String -> FileManagerMonad ()
fileRewrite name content = do
  time   <- ask
  curDir <- getCurDir
  file   <- dirFindFile name curDir
  checkWritable file
  checkReadable file
  let updFile = fileWrite time content file
  updCur <- dirInsertFile updFile curDir
  setCurDir updCur
  curPath <- gets (dirPath . curState)
  addAction $ Action.rewrite (curPath </> name) (fileContent updFile)

append :: String -> String -> FileManagerMonad ()
append name content = do
  time   <- ask
  curDir <- getCurDir
  file   <- dirFindFile name curDir
  let updFile = fileAppend time content file
  updCur <- dirInsertFile updFile curDir
  setCurDir updCur
  curPath <- gets (dirPath . curState)
  addAction $ Action.append (curPath </> name) (toFileContent content)

cat :: String -> FileManagerMonad ByteString
cat name = do
  curDir <- getCurDir
  file   <- dirFindFile name curDir
  checkReadable file
  return $ fileContent file

findFileInCvs :: String -> Int -> FileManagerMonad File
findFileInCvs name v = do
  let cvsFileName = getNameWithVersion name v
  cvs  <- getCvsDir
  dirFindFile cvsFileName cvs
  `catchError`
  \_ -> throwError $ NoFileVersion name v


cvsFileVersion :: String -> Int -> FileManagerMonad ByteString
cvsFileVersion name v
  | v < 0     = throwError $ InvalidVersion v
  | otherwise = do
      file <- findFileInCvs name v
      return $ fileContent file



cvsDeleteVersion :: String -> Int -> FileManagerMonad ()
cvsDeleteVersion name v
  | v < 0     = throwError $ InvalidVersion v
  | otherwise = do
    let vName = getNameWithVersion name v
    fileCvsDelete vName
    fileCvsDelete (nameWithMsg vName)

cvsUpdateFile :: String -> String -> FileManagerMonad ()
cvsUpdateFile name msg = do
  cur     <- getCurDir
  cvs     <- getCvsDir
  file    <- dirFindFile name cur
  cvsFile <- dirFindFile (nameInCvs name) cvs
             `catchError`
             \_ -> throwError (FileNotInCvs name)
  lastV <- case readMaybe (stringContent cvsFile) of
             Just  v -> return v
             Nothing -> throwError CvsFailure
  let v     = lastV + 1
  let vName = getNameWithVersion name v
  catchError
     do { makeCvsFile vName (fileContent file)
        ; makeCvsFile (nameWithMsg vName) (toFileContent msg)
        ; fileCvsRewrite (nameInCvs name) (show v)
        }
     \_ -> throwError CvsFailure

removeFile :: String -> FileManagerMonad ()
removeFile name = do
  cur    <- getCvsDir
  updDir <- dirDeleteFile name cur
  setCurDir updDir

mergeDiff :: String -> Int -> Int -> FileManagerMonad [Diff [String]]
mergeDiff name v1 v2 = do
  file1    <- findFileInCvs name v1
  file2    <- findFileInCvs name v2
  let splited1 = splitOn "\n" (stringContent file1)
  let splited2 = splitOn "\n" (stringContent file2)
  return $ getGroupedDiff splited1 splited2

mergeBothAllInfo :: String -> Int -> Int -> FileManagerMonad ()
mergeBothAllInfo name v1 v2 = do
  diff <- mergeDiff name v1 v2
  let merged = concat $ map diffMapper diff
  fileRewrite name $ intercalate "\n" merged
  where
    arrows = ">>>>>>>"
    diffMapper = \case
      First  l -> [arrows ++ " first begin "] ++ l ++ [arrows ++ " first end "]
      Second l -> [arrows ++ " second begin"] ++ l ++ [arrows ++ " second end"]
      Both _ l -> l


data MergeWay = BothWay | BothAllWay | FirstWay | SecondWay | Inter

withArrows :: [String] -> [String] -> [String]
withArrows l r = arrows ++ l ++ arrows ++ r ++ arrows
  where arrows = [">>>>>>>"]


merge :: String -> Int -> Int -> MergeWay -> FileManagerMonad ()
merge name v1 v2 BothAllWay = mergeBothAllInfo name v1 v2
merge name v1 v2 way = do
  diff <- mergeDiff name v1 v2
  let merged = concat $ map mapper $ smartMapper diff
  fileRewrite name $ intercalate "\n" merged
  where
    mapper = case way of
      BothWay -> \case
        Right l     -> l
        Left (l, r) -> withArrows l r
      FirstWay -> \case
        Right list  -> list
        Left (l, r) -> l
      SecondWay -> \case
        Right list  -> list
        Left (l, r) -> r

mergeInteractive :: String -> Int -> Int -> FileManagerMonad [Either String String]
mergeInteractive name v1 v2 = do
  diff <- mergeDiff name v1 v2
  return $ map mapper $ smartMapper diff
  where
    inter  = intercalate "\n"
    mapper (Right list)  = Right $ inter list
    mapper (Left (l, r)) = Left $ inter $ withArrows l r


data UsrMergeChoice = F | S | FS | SF | BOTH

matchConflicts :: [Either ([String], [String]) [String]] -> [UsrMergeChoice] -> FileManagerMonad [String]
matchConflicts [] [] = return []
matchConflicts [] _  = throwError MergeChoicesDontMatch
matchConflicts (Right list:xs) c       = matchConflicts xs c  >>= return . (++) list
matchConflicts (Left (l, r):xs) []     = throwError MergeChoicesDontMatch
matchConflicts (Left (l, r):xs) (c:cs) = matchConflicts xs cs >>= return . (++) resolved
  where
    resolved = case c of
      F    -> l
      S    -> r
      FS   -> l ++ r
      SF   -> r ++ l
      BOTH -> withArrows l r

resolveMerge :: String -> Int -> Int -> [UsrMergeChoice] -> FileManagerMonad ()
resolveMerge name v1 v2 choices = do
  diff     <- mergeDiff name v1 v2
  resolved <- matchConflicts (smartMapper diff) choices
  fileRewrite name $ intercalate "\n" resolved

extractDiff :: Diff [a] -> [a]
extractDiff = \case
  Both _ l -> l
  First  l -> l
  Second l -> l

smartMapper :: [Diff [a]] -> [Either ([a], [a]) [a]]
smartMapper []              = []
smartMapper (x:[])          = [Right (extractDiff x)]
smartMapper (x:Both _ l:xs) = [Right (extractDiff x ++ l)] ++ smartMapper xs
smartMapper (Both _ l:xs)   = (Right l):(smartMapper xs)
smartMapper (x:y:rest)      = [Left (extractDiff x, extractDiff y)] ++ smartMapper rest

root :: FileManagerMonad String
root = gets rootPath

fileCurDelete :: String -> FileManagerMonad ()
fileCurDelete name = do
  curDir <- getCurDir
  cur    <- gets curState
  file   <- dirFindFile   name curDir
  updCur <- dirDeleteFile name curDir
  setCvsDir updCur
  addAction $ Action.deleteFile (dirPath cur </> name)

fileCvsDelete :: String -> FileManagerMonad ()
fileCvsDelete name = do
  cvsDir  <- getCvsDir
  cvs     <- getCvsState
  file    <- dirFindFile   name cvsDir
  updCvs  <- dirDeleteFile name cvsDir
  setCvsDir updCvs
  addAction $ Action.deleteFile (dirPath cvs </> name)

cvsRemoveFile :: String -> FileManagerMonad ()
cvsRemoveFile name = do
  cvs <- getCvsDir
  file <- dirFindFile (nameInCvs name) cvs
          `catchError`
          \_ -> throwError $ FileNotInCvs name
  let names = satisfyComponents keyPred cvs
  forM names $ \name ->
          fileCvsDelete name
          `catchError`
          \_ -> throwError CvsDeleteFailure
  return ()
  where
    reg       = mkRegex $ "^" ++ (nameInCvs name) ++ "(_[0-9]+(_msg)?)?$"
    keyPred s = case matchRegex reg s of
      Just  _ -> True
      Nothing -> False

findRecInSubdirs :: String -> Directory -> FileManagerMonad (Maybe File)
findRecInSubdirs name dir = do
  catchError do
       { file <- dirFindFile name dir
       ; return (Just file) }
    \_ -> do
       { let dirs = innerDirs dir
       ; mbs <- sequence $ map (findRecInSubdirs name) dirs
       ; return $ foldl (<|>) Nothing mbs }

findFileInSubdirs :: String -> FileManagerMonad ByteString
findFileInSubdirs name = do
  cur <- getCurDir
  mbFile <- findRecInSubdirs name cur
  case mbFile of
    Just  x -> return $ fileContent x
    Nothing -> throwError $ FileNotFound name

--historyInSubdirs :: Directory -> FileManagerMonad [String]
--historyInSubdirs dir = do
--  let files = innerFiles dir
--  let dirs  = filter ((==) cvsDirName . getName) $ innerDirs  dir
--  --history  <- sequence $ map filesAction files
--  catchError do
--    { h <- sequence $ map dirAction dirs
--    ; return $ concat h }
--    --; return $ filter (not . null) $ catMaybes history ++ concat h }
--    (\e-> throwError  e) -- CvsFailure
--  where
--    filesAction file  = catchError
--      (Just <$> cvsFileHistory (fileName file))
--      (\_ -> return Nothing)
--    dirAction innerDir = do {
--      ;cd (dirName dir)
--      ;list <- historyInSubdirs innerDir
--      ;cd ".."
--      ;return ((dirName dir):list)}
--
--cvsShowAll :: FileManagerMonad String
--cvsShowAll = do
--  cur     <- getCurDir
--  history <- historyInSubdirs cur
--  return $ intercalate "\n" history

