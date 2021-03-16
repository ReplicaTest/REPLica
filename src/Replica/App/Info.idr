module Replica.App.Info

import Control.App
import Control.App.Console

import Replica.App.FileSystem
import Replica.App.Replica
import Replica.Command.Info
import Replica.Core
import Replica.Other.String

data InfoContext : Type where

displayTests : Has [ State InfoContext InfoAction, Console] e =>
  Test -> App e ()
displayTests t = do
  putStrLn "❓ \{t.name}:"
  traverse_ (putStrLn . withOffset 4) t.description
  putStrLn $ withOffset 4 "- Command : \{show t.command}"
  putStrLn ""

export
infoReplica :
  FileSystem (FSError :: e) =>
  Has
    [ State InfoContext InfoAction
    , Exception ReplicaError
    , Console
    ] e => App e ()
infoReplica = do
  repl <- getReplica InfoContext file
  putStrLn ""
  traverse_ displayTests repl.tests
