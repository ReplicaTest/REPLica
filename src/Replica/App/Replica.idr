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
import Replica.Other.String
import Replica.Other.Validation


%default total

export
data CurrentTest : Type where

export
data ReplicaDir : Type where

public export
data ReplicaError
  = InaccessTestFile String
  | InvalidJSON (List String)

export
getOutputFile : Has
  [ State CurrentTest Test
  , State ReplicaDir String
  , FileSystem] e => App e String
getOutputFile = do
  d <- get ReplicaDir
  t <- get CurrentTest
  pure $ d </> defaultOutput t

export
getExpectedFile : Has
  [ State CurrentTest Test
  , State ReplicaDir String
  , FileSystem] e => App e String
getExpectedFile = do
  d <- get ReplicaDir
  t <- get CurrentTest
  pure $ d </> defaultExpected t

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

