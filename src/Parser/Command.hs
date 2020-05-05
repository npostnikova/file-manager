module Parser.Command
  ( Command (..)
  ) where

import FileManager.Monad (MergeWay)

type Name     = String
type FileName = String
type DirName  = String

data Command
  = Cd               FilePath
  | Cat              String
  | Ls               FilePath
  | Tree             DirName
  | Mkdir            DirName
  | Mkfile           FileName
  | Remove           Name
  | Information      FilePath
  | CvsInit
  | CvsAdd           Name
  | CvsUpdate        FileName String
  | CvsHistory       FileName
  | CvsRemove        FileName
  | CvsVersion       FileName Int
  | CvsDeleteVersion FileName Int
  | CvsShowAll
  | WriteFile        FileName String
  | AppendFile       FileName String
  | Merge            FileName Int Int MergeWay
  | FindFile         FileName
  | Root
  | Exit