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
import Replica.Other.String
import Replica.Other.Validation


%default total

export
data CurrentTest : Type where

export
data GlobalConfig : Type where

public export
data ReplicaError
  = InaccessTestFile String
  | InvalidJSON (List String)

export
testDir : String -> String
testDir = (</> "test")

export
setAbsoluteReplicaDir : Has [State GlobalConfig GlobalOption, FileSystem] e => App e ()
setAbsoluteReplicaDir = do
  rdir <- map replicaDir $ get GlobalConfig
  if isAbsolute rdir
     then pure ()
     else do
       pwd <- getCurrentDir
       modify GlobalConfig (record {replicaDir = pwd </> rdir})

export
getOutputFile : Has
  [ State CurrentTest Test
  , State GlobalConfig GlobalOption ] e => App e String
getOutputFile = do
  d <- map replicaDir $ get GlobalConfig
  t <- get CurrentTest
  pure $ testDir d </> defaultOutput t

export
getExpectedFile : Has
  [ State CurrentTest Test
  , State GlobalConfig GlobalOption ] e => App e String
getExpectedFile = do
  d <- map replicaDir $ get GlobalConfig
  t <- get CurrentTest
  pure $ testDir d </> defaultExpected t

export
getReplica :
  FileSystem (FSError :: e) => (0 ident : Type) ->
  Has [ State ident t
      , Exception ReplicaError ] e => (t -> String) -> App e Replica
getReplica ident toFile = do
  ctx <- get ident
  let file = toFile ctx
  content <- handle (readFile file)
                    pure
                    (\err : FSError => throw $ InaccessTestFile file)
  let Just json = parse content
        | Nothing => throw $ InvalidJSON []
  let Valid repl = jsonToReplica json
        | Error xs => throw $ InvalidJSON xs
  pure repl

