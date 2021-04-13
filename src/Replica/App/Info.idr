module Replica.App.Info

import Control.ANSI
import Control.App
import Control.App.Console

import Data.List
import Data.List1
import Data.String

import Language.JSON

import Replica.App.FileSystem
import Replica.App.Format
import Replica.App.Log
import Replica.App.Replica
import Replica.Command.Info
import Replica.Core
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Other.Decorated
import Replica.Other.String
import Replica.Other.Validation

data InfoContext : Type where

displayTestName : Console e =>
  State GlobalConfig Global e =>
  String -> App e ()
displayTestName x = putStrLn $ "\{!qmark} " ++ (!bold "\{x}:")

displayExpectation : FileSystem (FSError :: e) =>
  Has [ State InfoContext InfoCommand
      , State GlobalConfig Global
      , State CurrentTest Test
      , Console] e => App e ()
displayExpectation = do
  t <- get CurrentTest
  handle (readFile !getExpectedFile)
    (\o => do putStrLn $ withOffset 4 $ "Expected:"
              putStrLn !(expectation o))
    (\err : FSError => putStrLn "No expectation yet.")
  where
    expectation : String -> App e String
    expectation o =
      pure . unlines . map (!blue . withOffset 6) . forget $ lines o

filterTests : FileSystem (FSError :: e) =>
  Has [ State InfoContext InfoCommand
      , State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => (s : List Test) -> App e (List Test)
filterTests s = do
  f <- filter <$> get InfoContext
  debug $ "Filters: \{show f}"
  pure $ filter (keepTest f) s

getLastFailures : FileSystem (FSError :: e) =>
  Has [ State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => App e (List Test)
getLastFailures = do
  repl <- getReplica
  logFile <- lastRunLog <$> getReplicaDir
  lastLog <- catchNew (readFile logFile)
    (\err : FSError => throw $ CantAccessTestFile logFile)
  let Just json = parse lastLog
    | Nothing => throw $ InvalidJSON []
  let Valid report = parseReport json
    | Error err => throw $ InvalidJSON err
  let notWorking = fst <$> filter (not . isSuccess . snd) report
  pure $ filter (flip elem notWorking . name) repl.tests

defineActiveTests : FileSystem (FSError :: e) =>
  Has [ State InfoContext InfoCommand
      , State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => App e (List Test)
defineActiveTests = do
  last <- if !((.filter.lastFailures) <$> get InfoContext)
        then getLastFailures
        else do
          repl <- getReplica
          pure repl.tests
  filterTests last

displayTests : FileSystem (FSError :: e) =>
  Has [ State InfoContext InfoCommand
      , State GlobalConfig Global
      , State CurrentTest Test
      , Console] e =>
  App e ()
displayTests = do
  t <- get CurrentTest
  displayTestName t.name
  traverse_ (putStrLn . withOffset 4) t.description
  when (not $ null t.tags)
    $ putStrLn . withOffset 4 $ "Tags: \{show t.tags}"
  when (not $ null t.require)
    $ putStrLn . withOffset 4 $ "Require: \{show t.require}"
  putStrLn $ withOffset 4 "Command : \{show t.command}"
  when !(showExpectation <$> get InfoContext)
    displayExpectation
  putStrLn ""

export
infoReplica :
  FileSystem (FSError :: e) =>
  Has
    [ State InfoContext InfoCommand
    , State GlobalConfig Global
    , Exception ReplicaError
    , Console
    ] e => App e ()
infoReplica = do
  debug $ "Info: \{show !(get InfoContext)}"
  debug $ show !(get GlobalConfig)
  putStrLn ""
  tests <- defineActiveTests
  traverse_ (\t => new t displayTests) tests
