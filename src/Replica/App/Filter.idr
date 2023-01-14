module Replica.App.Filter

import Control.App
import Control.App.Console

import Data.List

import Language.JSON

import Replica.App.FileSystem
import Replica.App.Log
import Replica.App.Replica

import Replica.Core.Parse
import Replica.Core.Types
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Other.Decorated
import Replica.Other.Validation

%default total

filterTests : FileSystem (FSError :: e) =>
  Has [ State ActiveFilter Filter
      , State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => (s, r : List Test) -> App e (List Test, List Test)
filterTests s r = do
  activeFilters <- get ActiveFilter
  debug $ "Filters: \{show activeFilters}"
  let (selected, rejected) = partition (keepTest activeFilters) s
  pure (selected, rejected ++ r)

getLastFailures : FileSystem (FSError :: e) =>
  Has [ State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => App e (List Test, List Test)
getLastFailures = do
  repl <- getReplica
  logFile <- lastRunLog <$> getReplicaDir
  lastLog <- catchNew (readFile logFile)
    (\err : FSError => throw $ CantAccessTestFile logFile)
  let Just json = parse lastLog
    | Nothing => throw $ InvalidJSON ["Can't parse JSON (invalid syntax)"]
  let Valid report = parseReport json
    | Error err => throw $ InvalidJSON err
  let notWorking = fst <$> filter (not . isFullSuccess . snd) report
  let (selected, rejected) = partition (flip elem notWorking . name) repl.tests
  debug $ "Previous invalid tests: \{show selected}"
  pure (selected, rejected)

export
defineActiveTests : FileSystem (FSError :: e) =>
  Has [ State ActiveFilter Filter
      , State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => App e (List Test, List Test)
defineActiveTests = do
  tests <- if (!(get ActiveFilter)).lastFailures
        then getLastFailures
        else do
          repl <- getReplica
          pure (repl.tests, [])
  uncurry filterTests tests
