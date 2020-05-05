{-# LANGUAGE LambdaCase #-}

module Parser.CommandParser
  ( commandParser
  ) where

import           Data.Semigroup        ((<>))
import           FileManager.Monad     (ExecResult, FileManagerMonad,
                                        MergeWay (..), MyState,
                                        UsrMergeChoice (..))
import           Options.Applicative   (Parser, ParserInfo, ParserResult (..),
                                        argument, auto, command, defaultPrefs,
                                        fullDesc, header, help, helper,
                                        hsubparser, info, maybeReader, metavar,
                                        progDesc, strArgument, value)
import           Parser.Command        (Command (..))


import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8


textAboutPath :: String
textAboutPath = "path relative to current dir"
             ++ " or path from file manager root if starts with \"/\""

cdArguments :: Parser Command
cdArguments = Cd <$> strArgument
  (  metavar "<dirpath>"
  <> help textAboutPath )

catArguments :: Parser Command
catArguments = Cat <$> strArgument
  (  metavar "<filename>"
  <> help "file in current directory" )

lsArguments :: Parser Command
lsArguments = Ls <$> strArgument
  (  metavar "<dirpath>"
  <> value ""
  <> help textAboutPath )

treeArguments :: Parser Command
treeArguments = Tree <$> strArgument
  (  metavar "<dirpath>"
  <> value ""
  <> help textAboutPath )

mkdirArguments :: Parser Command
mkdirArguments = Mkdir <$> strArgument
  (  metavar "<dirname>"
  <> help "dir to create in current directory" )

mkfileArguments :: Parser Command
mkfileArguments = Mkfile <$> strArgument
  (  metavar "<filename>"
  <> help "file to create in current directory" )

removeArguments :: Parser Command
removeArguments = Remove <$> strArgument
  (  metavar "<filename>"
  <> help "file in current directory" )

infoArguments :: Parser Command
infoArguments = Information <$> strArgument
  (  metavar "<filepath | dirpath>"
  <> value ""
  <> help textAboutPath )

initArguments :: Parser Command
initArguments = pure CvsInit

addArguments :: Parser Command
addArguments = CvsAdd <$> strArgument
  (  metavar "<filename>"
  <> help "file in current directory" )

updateArguments :: Parser Command
updateArguments = CvsUpdate
  <$> strArgument
     (  metavar "<filename>"
     <> help "file in current directory to commit" )
  <*> strArgument
     (  metavar "<message>"
     <> help "comment for changes" )

historyArguments :: Parser Command
historyArguments = CvsHistory <$> strArgument
  (  metavar "<filename>"
  <> help "file in cur dir to show history of" )

cvsRemoveArguments :: Parser Command
cvsRemoveArguments = CvsRemove <$> strArgument
  (  metavar "<filename>"
  <> help "file (is/was) in cur dir" )

versionArguments :: Parser Command
versionArguments = CvsVersion
  <$> strArgument
      (  metavar "<filename>"
      <> help "file in cur dir" )
  <*> argument auto
      (  metavar "<version>"
      <> help "version to show")

deleteVersionArguments :: Parser Command
deleteVersionArguments = CvsDeleteVersion
  <$> strArgument
      (  metavar "<filename>"
      <> help "file in cur dir" )
  <*> argument auto
      (  metavar "<version>"
      <> help "version to delete")

writeArguments :: Parser Command
writeArguments = WriteFile
  <$> strArgument
      (  metavar "<filename>"
      <> help "file in cur dir" )
  <*> strArgument
      (  metavar "<text>"
      <> help "text to write, which don't contain quotes inside :)" )

findFileArguments :: Parser Command
findFileArguments = FindFile
  <$> strArgument
      (  metavar "<filename>"
      <> help "file to find in subdirs of current directory" )

--cvsAllArguments :: Parser Command
--cvsAllArguments = pure CvsShowAll

appendArguments :: Parser Command
appendArguments = AppendFile
  <$> strArgument
      (  metavar "<filename>"
      <> help "file in cur dir" )
  <*> strArgument
      (  metavar "<text>"
      <> help "text to append, which don't contain quotes inside :)" )

mergeArguments :: Parser Command
mergeArguments = Merge
  <$> strArgument
      (  metavar "<filename>"
      <> help "file in cur dir" )
  <*> argument auto
      (  metavar "<version>"
      <> help "version to merge")
  <*> argument auto
      (  metavar "<version>"
      <> help "version to merge")
  <*> (argument (maybeReader $
         \case
           "first"       -> Just FirstWay
           "second"      -> Just SecondWay
           "both"        -> Just BothWay
           "both-all"    -> Just BothAllWay
           "interactive" -> Just Inter
           _ -> Nothing )
      (  metavar "<first | second | both | both-all | interactive>"
      <> help "way of merging") )

rootArguments :: Parser Command
rootArguments = pure Root

exitArguments :: Parser Command
exitArguments = pure Exit

commands :: Parser Command
commands = hsubparser
  (  command "cd"
     (info cdArguments (progDesc "Change current directory path"))
  <> command "cat"
     (info catArguments (progDesc "Show content of file in cur dir"))
  <> command "ls"
     (info lsArguments (progDesc "Show content of directory. Current by default."))
  <> command "tree"
     (info treeArguments (progDesc "Show tree of directory content. Current by default."))
  <> command "mkdir"
     (info mkdirArguments (progDesc "Create directory in current dir"))
  <> command "mkfile"
     (info mkfileArguments (progDesc "Create file in current dir"))
  <> command "remove"
     (info removeArguments (progDesc "Remove file from current dir"))
  <> command "information"
     (info infoArguments (progDesc "Get information about file or dir. Current dir by default"))
  <> command "cvs-init"
     (info initArguments (progDesc "Init cvs in current dir"))
  <> command "cvs-add"
     (info addArguments (progDesc "Add to cvs file which is in cur dir"))
  <> command "cvs-update"
     (info updateArguments (progDesc "Commit changes of file in cur dir"))
  <> command "cvs-history"
     (info historyArguments (progDesc "Get cvs-history of file in cur dir"))
  <> command "cvs-remove"
     (info cvsRemoveArguments (progDesc "Remove file, which was/is in cur dir, from cvs"))
  <> command "cvs-version"
     (info versionArguments (progDesc "Get version of file which is in cur dir"))
  <> command "cvs-delete-version"
     (info deleteVersionArguments (progDesc "Delete version of file which is in cur dir"))
  <> command "write"
     (info writeArguments (progDesc "Rewrite file in current directory"))
  <> command "append"
     (info appendArguments (progDesc "Append to file in current directory"))
  <> command "merge"
     (info mergeArguments (progDesc "Merge versions of file in current directory and write the result in the file."))
  <> command "find-file"
     (info findFileArguments (progDesc "Find file in subdirs of current directory"))
  <> command "root"
     (info rootArguments (progDesc "Get path of root directory"))
  <> command "exit"
     (info exitArguments (progDesc "Good way to leave the app"))
  )

headerText :: String
headerText = "Hey!\n\
  \It\'s my amazing (not really) file manager, which handles a plenty of commands.\n\
  \My file manager -- my rules:\n\
  \\t1. You can\'t go out of root dir. No way.\n\
  \\t2. Most actions only work when you specify entity in current directory. Be cautious.\n"
commandParser :: ParserInfo Command
commandParser = info
  (helper  <*> commands)
  (fullDesc <> progDesc "Check help for help" <> header headerText)
