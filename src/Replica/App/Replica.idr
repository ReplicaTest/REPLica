module Replica.App.Replica

import Control.App
import Control.App.Console

import Data.List
import Data.Maybe
import Data.String

import Language.JSON

import System.Path

import Replica.App.FileSystem
import Replica.App.System
import Replica.Command.Run
import Replica.Core.Parse
import Replica.Core.Types
import Replica.Option.Global
import Replica.Other.Decorated
import Replica.Other.String
import Replica.Other.Validation


%default total

export
data CurrentTest : Type where

export
data GlobalConfig : Type where

public export
data ReplicaError
  = CantAccessTestFile String
  | InvalidJSON (List String)

export
Show ReplicaError where
  show (CantAccessTestFile x) = "Can't access file \{x}"
  show (InvalidJSON xs) = removeTrailingNL $ unlines $ "Can't parse JSON:" ::xs

export
testDir : String -> String
testDir = (</> "test")

export
logDir : String -> String
logDir = (</> "log")

export
lastRunLog : String -> String
lastRunLog rdir = logDir rdir </> "last.json"

export
getReplicaDir : State GlobalConfig Global e => App e String
getReplicaDir = replicaDir <$> get GlobalConfig

export
getGoldenDir : State GlobalConfig Global e => App e String
getGoldenDir = do
  gd <- goldenDir <$> get GlobalConfig
  maybe (testDir <$> getReplicaDir) pure gd

export
setAbsoluteReplicaDir : Has [State GlobalConfig Global, FileSystem] e => App e ()
setAbsoluteReplicaDir = do
  rdir <- getReplicaDir
  if isAbsolute rdir
     then pure ()
     else do
       pwd <- getCurrentDir
       modify GlobalConfig ({replicaDir := pwd </> rdir})

export
getSingleTestDir : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => App e String
getSingleTestDir = do
  d <- getReplicaDir
  t <- get CurrentTest
  pure $ testDir d </> t.name

export
getSingleTestFileDir : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => App e String
getSingleTestFileDir = (</> defaultFile) <$> getSingleTestDir

export
getSingleTestGoldenDir : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => App e String
getSingleTestGoldenDir = do
  d <- getGoldenDir
  t <- get CurrentTest
  pure $ d </> t.name

export
getSingleTestGoldenFileDir : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => App e String
getSingleTestGoldenFileDir = (</> defaultFile) <$> getSingleTestGoldenDir

export
getErrorFile : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => App e String
getErrorFile = do
  t <- getSingleTestDir
  pure $ t </> defaultError

export
getInputFile : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => App e String
getInputFile = do
  t <- getSingleTestDir
  pure $ t </> defaultInput

export
getOutputFile : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => App e String
getOutputFile = do
  t <- getSingleTestDir
  pure $ t </> defaultOutput

export
getStatusFile : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => App e String
getStatusFile = do
  t <- getSingleTestDir
  pure $ t </> defaultStatus

export
getWatchedFile : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => String -> App e String
getWatchedFile f = do
  t <- get CurrentTest
  pure $ maybe f (</> f) t.workingDir

export
getExpectedOutput : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => App e String
getExpectedOutput = do
  t <- getSingleTestGoldenDir
  pure $ t </> defaultExpectedOutput

export
getExpectedError : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => App e String
getExpectedError = do
  t <- getSingleTestGoldenDir
  pure $ t </> defaultExpectedError

export
getExpectedFile : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => String -> App e String
getExpectedFile s = do
  t <- getSingleTestGoldenFileDir
  pure $ t </> s

export
getReplica :
  FileSystem (FSError :: e) =>
  Has [ State GlobalConfig Global
      , Exception ReplicaError ] e => App e Replica
getReplica = do
  ctx <- get GlobalConfig
  let fs = ctx.files
  content <- traverse readReplica fs
  let Just jsons = traverse parse content
        | Nothing => throw $ InvalidJSON []
  let res = traverse jsonToReplica jsons
  let Valid repl = map (>>= (\(MkReplica xs) => xs)) res
        | Error xs => throw $ InvalidJSON xs
  let ([], _) = duplicatedKeys repl
      | (dup, _) => throw $ InvalidJSON ["Duplicated key(s): \{show dup}"]
  pure $ MkReplica repl
  where
    readReplica : String -> App e String
    readReplica f = handle (readFile f)
      pure (\err : FSError => throw $ CantAccessTestFile f)
    go : (List String, List String) -> Test -> (List String, List String)
    go (dup, names) t = if t.name `elem` names
                           then if t.name `elem` dup
                                   then (dup, names)
                                   else (t.name :: dup, names)
                           else (dup, t.name :: names)
    duplicatedKeys : List Test -> (List String, List String)
    duplicatedKeys xs = foldl go ([], []) xs

export
when : Bool -> App e () -> App e ()
when cond x = if cond then x else pure ()

export
catchNew : App (err :: e) a -> (err -> App e a) -> App e a
catchNew x f = handle x pure (\er : err => f er)
