module Replica.App.Info

import Control.App
import Control.App.Console

import Data.List1
import Data.String

import Replica.App.FileSystem
import Replica.App.Replica
import Replica.Command.Info
import Replica.Core
import Replica.Option.Global
import Replica.Other.String

data InfoContext : Type where

displayExpectation : FileSystem (FSError :: e) =>
  Has [ State InfoContext InfoAction
      , State GlobalConfig GlobalOption
      , State CurrentTest Test
      , Console] e => App e ()
displayExpectation = do
  t <- get CurrentTest
  f <- getExpectedFile
  handle (readFile f)
    (\o => do
      putStrLn $ withOffset 4 $ "Expected:"
      putStrLn $ unlines $ map (withOffset 6) $ forget $ lines o)
    (\err : FSError => putStrLn "No expectation yet.")

displayTests : FileSystem (FSError :: e) =>
  Has [ State InfoContext InfoAction
      , State GlobalConfig GlobalOption
      , State CurrentTest Test
      , Console] e =>
  App e ()
displayTests = do
  t <- get CurrentTest
  putStrLn "❓ \{t.name}:"
  traverse_ (putStrLn . withOffset 4) t.description
  when (not $ null t.tags)
    $ putStrLn . withOffset 4 $ "Tags: \{show t.tags}"
  when (not $ null t.require)
    $ putStrLn . withOffset 4 $ "Require: \{show t.require}"
  putStrLn $ withOffset 4 "Command : \{show t.command}"
  when !(map showExpectation $ get InfoContext)
    displayExpectation
  putStrLn ""

export
infoReplica :
  FileSystem (FSError :: e) =>
  Has
    [ State InfoContext InfoAction
    , State GlobalConfig GlobalOption
    , Exception ReplicaError
    , Console
    ] e => App e ()
infoReplica = do
  repl <- getReplica InfoContext file
  putStrLn ""
  traverse_ (\t => new t displayTests) repl.tests
