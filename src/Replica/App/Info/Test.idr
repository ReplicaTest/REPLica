module Replica.App.Info.Test

import Control.App
import Control.App.Console

import Data.List
import Data.List1
import Data.String
import Data.String.Extra

import Language.JSON

import Replica.App.Display
import Replica.App.FileSystem
import Replica.App.Format
import Replica.App.Log
import Replica.App.Replica
import Replica.Command.Info.Test
import Replica.Core
import Replica.Core.Test
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Other.Decorated
import Replica.Other.String
import Replica.Other.Validation

import Replica.App.Info.Types

displayTestName : Console e =>
  State GlobalConfig Global e =>
  String -> App e ()
displayTestName x = putStrLn $ "\{!qmark} " ++ (!bold "\{x}:")

displayExpectation : FileSystem (FSError :: e) =>
  Has [ State TestInfoContext TestInfoCommand
      , State GlobalConfig Global
      , State CurrentTest Test
      , Console] e => Expectation -> App e ()
displayExpectation exp = do
  let Generated = exp
    | Exact expected => printExpectation expected
    | Partial x xs => do
        putStrLn $ withOffset 6 $ case x of
          Ordered => "Expect these parts (ordered):"
          Whatever => "Expect these parts (in any order):"
        traverse_ putStrLn (map partialExpectation xs)
    | EndsWith x => do
      putStrLn $ withOffset 6 $ "Ends with: \{show x}"
    | StartsWith x => do
      putStrLn $ withOffset 6 $ "Starts with: \{show x}"
  handle (readFile !getExpectedOutput)
    printExpectation
    (\err : FSError => putStrLn "No expectation yet.")
  where
    printExpectation : String -> App e ()
    printExpectation o = do
      putStrLn $ withOffset 6 $ "Expect exactly as output:"
      putStrLn $ removeTrailingNL $ unlines $ map (withOffset 8) $ lines o
    partialExpectation : String -> String
    partialExpectation x = case lines x of
      (head :: tail) => removeTrailingNL $ unlines $ withOffset 6 ("- " ++ head) :: (withOffset 8 <$> tail)
      [] => withOffset 6 "- "

displayExpectations : FileSystem (FSError :: e) =>
  Has [ State TestInfoContext TestInfoCommand
      , State GlobalConfig Global
      , State CurrentTest Test
      , Console] e => App e ()
displayExpectations = do
  t <- get CurrentTest
  traverse_ (uncurry go) t.expectations
  where
    showPart : Part -> App e ()
    showPart StdOut = putStrLn $ withOffset 4 "Expected on standard output"
    showPart StdErr = putStrLn $ withOffset 4 "Expected on error output"
    showPart (FileName x) = putStrLn $ withOffset 4 "Expected in file \{show x}"
    go : Part -> List Expectation -> App e ()
    go x xs = do
      showPart x
      traverse_ displayExpectation xs


filterTests : FileSystem (FSError :: e) =>
  Has [ State TestInfoContext TestInfoCommand
      , State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => (s : List Test) -> App e (List Test)
filterTests s = do
  f <- filter <$> get TestInfoContext
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
  let notWorking = fst <$> filter (not . isFullSuccess . snd) report
  pure $ filter (flip elem notWorking . name) repl.tests

defineActiveTests : FileSystem (FSError :: e) =>
  Has [ State TestInfoContext TestInfoCommand
      , State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => App e (List Test)
defineActiveTests = do
  last <- if !((.filter.lastFailures) <$> get TestInfoContext)
        then getLastFailures
        else do
          repl <- getReplica
          pure repl.tests
  filterTests last

displayTest :
  FileSystem (FSError :: e) =>
  Has [ State TestInfoContext TestInfoCommand
      , State GlobalConfig Global
      , State CurrentTest Test
      , Console] e =>
  App e ()
displayTest = do
  t <- get CurrentTest
  displayTestName t.name
  traverse_ (putStrLn . withOffset 4) t.description
  when (not $ null t.tags)
    $ putStrLn . withOffset 4 $ "Tags: \{show t.tags}"
  when (not $ null t.require)
    $ putStrLn . withOffset 4 $ "Require: \{show t.require}"
  putStrLn $ withOffset 4 "Command : \{show t.command}"
  when !(showExpectation <$> get TestInfoContext)
    displayExpectations
  putStrLn ""

displayTestBySuite :
  FileSystem (FSError :: e) =>
  Has [ State TestInfoContext TestInfoCommand
      , State GlobalConfig Global
      , Console
      ] e =>
  (Maybe String, List1 Test) -> App e ()
displayTestBySuite (suite, tests) = do
  displaySuite suite
  traverse_ (\t => new t displayTest) tests


export
testInfoReplica :
  FileSystem (FSError :: e) =>
  Has
    [ State TestInfoContext TestInfoCommand
    , State GlobalConfig Global
    , Exception ReplicaError
    , Console
    ] e => App e ()
testInfoReplica = do
  debug "Info: \{show !(get TestInfoContext)}"
  debug $ show !(get GlobalConfig)
  putStrLn ""
  tests <- defineActiveTests
  traverse_ displayTestBySuite $ bySuite tests
