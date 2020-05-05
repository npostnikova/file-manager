{-# LANGUAGE LambdaCase #-}

module FileManager.Error
  ( FileManagerError (..)
  ) where

data FileManagerError
  = NotFound     String
  | DirNotFound  String
  | FileNotFound String
  | DirExpected  String
  | FileExpected String
  | DirExist String
  | FileExist String
  | EntityExist String
  | CvsInitialized
  | CvsNotInitialized
  | FileNotInCvs String
  | NoFileVersion String Int
  | InvalidVersion Int
  | CvsFailure
  | InvalidPathArg
  | FileManagerFailure
  | MergeChoicesDontMatch
  | CvsDeleteFailure
  | NoReadPermission String
  | NoWritePermission String

instance Show FileManagerError where
  show = \case
    NotFound            s -> s ++ " not found"
    DirNotFound         s -> s ++ " directory not found"
    FileNotFound        s -> s ++ " file not found"
    DirExpected         s -> s ++ " is not a directory"
    FileExpected        s -> s ++ " is not a file"
    DirExist            s -> s ++ " directory already exists"
    FileExist           s -> s ++ " file already exists"
    EntityExist         s -> s ++ " already exists"
    NoReadPermission    s -> s ++ " is not readable"
    NoWritePermission   s -> s ++ " is not writable"
    FileNotInCvs        s -> s ++ " is not added to cvs"
    InvalidVersion      i -> show i ++ " is invalid version number"
    NoFileVersion     s i -> s ++ " does not have version with number " ++ show i
    CvsInitialized        -> "directory is already under cvs"
    CvsNotInitialized     -> "cvs is not initialized"
    CvsFailure            -> "cvs representation is invalid"
    FileManagerFailure    -> "blah blah invalid"
    InvalidPathArg        -> "invalid path provided"
    CvsDeleteFailure      -> "something went wrong while deleting from cvs"
    MergeChoicesDontMatch -> "failed to resolve interactive merge"
