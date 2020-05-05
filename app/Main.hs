{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import           FileManager.Monad          (DirectoryState (..), ExecResult,
                                             FileManagerMonad, MergeWay (..),
                                             MyState (..), UsrMergeChoice (..),
                                             append, cat, cd, cvsAddFile,
                                             cvsDeleteVersion, cvsFileHistory,
                                             cvsFileVersion, cvsInit,
                                             cvsRemoveFile, cvsUpdateFile,
                                             fileCurDelete, fileRewrite,
                                             findFileInSubdirs, getCurPath,
                                             information, ls, merge,
                                             mergeInteractive, mkdir, mkfile,
                                             resolveMerge, root, tree)
import           FileSystem.Reader          (readDirectory)
import           Parser.Command             (Command (..))
import           Parser.CommandParser       (commandParser)
import qualified Data.ByteString            as S
import qualified Data.ByteString.Char8      as S8
import           Parser.InputSplitter       (splitLine)
import           Options.Applicative        (ParserResult (..), defaultPrefs,
                                             execParserPure, renderFailure)
import           Data.Semigroup             ((<>))
import           System.Environment         (getArgs)
import           Control.Monad.Error.Class  (catchError)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Time.Clock            (getCurrentTime)
import           Control.Monad.State        (State, runState)
import           Control.Monad.Trans.Reader (runReaderT)
import           System.IO                  (hFlush, putStr, putStrLn, stdout)
import           NameManip                  (slicePath)

getResult :: MyState -> FileManagerMonad r -> IO (ExecResult r)
getResult state monad = do
  time <- getCurrentTime
  return $ runState (runExceptT (runReaderT monad time)) state

stringHandler :: String -> IO ()
stringHandler = putStrLn

bsHandler :: S.ByteString -> IO ()
bsHandler = S8.putStrLn

emptyHandler :: () -> IO ()
emptyHandler = return

handleBS :: MyState -> FileManagerMonad S.ByteString -> IO ()
handleBS = handleResult bsHandler

handleString :: MyState -> FileManagerMonad String -> IO ()
handleString = handleResult stringHandler

handleEmpty :: MyState -> FileManagerMonad () -> IO ()
handleEmpty = handleResult emptyHandler

handleResult ::  (r -> IO ()) -> MyState -> FileManagerMonad r -> IO ()
handleResult handler state monad = do
  time <- getCurrentTime
  (result, st) <- getResult state monad
  newState <-
    case result of
      Left err -> do
        putStrLn $ show err
        return state
      Right s  -> do
        handler s
        return st
  iteration newState

handleExit :: MyState -> IO ()
handleExit state = do
  catchError
    (sequence_ (actions state))
    \_ -> putStrLn "Failed to write the changes."

getChoice :: IO (Maybe UsrMergeChoice)
getChoice = do
  c      <- getLine
  case c of
    "q"  -> return Nothing
    "f"  -> return $ Just F
    "s"  -> return $ Just S
    "fs" -> return $ Just FS
    "sf" -> return $ Just SF
    "b"  -> return $ Just BOTH
    _    -> do
      putStrLn "print q if you want to leave"
      getChoice

getUsrChoices :: [Either String String] -> IO (Maybe [UsrMergeChoice])
getUsrChoices [] = return $ Just []
getUsrChoices (Right s:xs) = do
  putStrLn s
  getUsrChoices xs
getUsrChoices (Left s:xs) = do
  putStrLn s
  choice <- getChoice
  case choice of
    Nothing -> return Nothing
    Just c  -> do
      rest <- getUsrChoices xs
      return $ (:) c <$> rest

handleMergeInteractive :: MyState -> String -> Int -> Int -> IO ()
handleMergeInteractive state name v1 v2 = do
  (eit, st) <- getResult state $ mergeInteractive name v1 v2
  case eit of
    Left err -> do
      putStrLn $ show err
      iteration state
    Right list -> do
      choicesMb <- getUsrChoices list
      case choicesMb of
        Nothing -> return ()
        Just ch -> handleEmpty state $ resolveMerge name v1 v2 ch

handleCommand :: MyState -> Command -> IO ()
handleCommand state = \case
  Cd   path               -> handleEmpty  state $ cd   path
  Cat  name               -> handleBS     state $ cat  name
  Ls   path               -> handleString state $ ls   path
  Tree path               -> handleString state $ tree path
  Mkdir name              -> handleEmpty  state $ mkdir name
  Mkfile name             -> handleEmpty  state $ mkfile name
  Remove name             -> handleEmpty  state $ fileCurDelete name
  CvsRemove name          -> handleEmpty  state $ cvsRemoveFile name
  Information name        -> handleString state $ information name
  CvsInit                 -> handleEmpty  state $ cvsInit
  CvsAdd name             -> handleEmpty  state $ cvsAddFile name
  CvsUpdate name msg      -> handleEmpty  state $ cvsUpdateFile name msg
  CvsHistory name         -> handleString state $ cvsFileHistory name
  CvsDeleteVersion name v -> handleEmpty  state $ cvsDeleteVersion name v
  CvsVersion name v       -> handleBS     state $ cvsFileVersion name v
  WriteFile name text     -> handleEmpty  state $ fileRewrite name text
  AppendFile name text    -> handleEmpty  state $ append name text
  Merge name v1 v2 Inter  -> handleMergeInteractive state name v1 v2
  Merge name v1 v2 way    -> handleEmpty  state $ merge name v1 v2 way
  FindFile name           -> handleBS     state $ findFileInSubdirs name
  Root                    -> handleString state $ root
  Exit                    -> handleExit   state

parseArgs :: MyState -> [String] -> IO ()
parseArgs state list = do
  let result = execParserPure defaultPrefs commandParser list
  case result of
    Failure failure -> do
      putStrLn $ fst $ renderFailure failure ""
      iteration state
    Success comm    -> do
      handleCommand state comm

iteration :: MyState -> IO ()
iteration state = do
  putStr $ "~" ++ getCurPath state ++ "> "
  hFlush stdout
  line    <- getLine
  case splitLine line of
    Left err   -> putStrLn err >> iteration state
    Right list -> parseArgs state list

main :: IO ()
main = do
  args <- getArgs
  case args of
    []       -> putStrLn "Please, pass absolute path as a command line argument"
    (path:_) -> do
      let slPath = slicePath path
      catchError
        do { dir <- readDirectory path
           ; let iniState = MyState { rootDirectory  = dir
                                  , rootPath       = path
                                  , slicedRootPath = slPath
                                  , curState       = DirectoryState path []
                                  , cvsStateMb     = Nothing
                                  , actions        = [] }
           ; parseArgs iniState ["cd", ""] }
        \_ -> putStrLn "Failed to read the directory." >>
              putStrLn "Please, provide a correct path and don't change it during execution."
