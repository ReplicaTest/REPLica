||| Some useful methods for test manipulation
module Replica.Core.Test

import Data.List
import Data.List1

import Replica.Core.Types

export
||| Group tests by their suite name. Takes a list of Test and returns a list of pairs (Maybe String, List1 Test)
||| where the Maybe String is the suite name (Nothing if absent) and the List1 contains tests in that suite.
bySuite : List Test -> List (Maybe String, List1 Test)
bySuite = let
  withName : List1 Test -> (Maybe String, List1 Test)
  withName xs@(x:::_) = (x.suite, xs)
  in map withName . groupBy ((==) `on` suite) . sortBy (compare `on` suite)

export
||| Group tests by their tags. Takes a list of Test and returns a list of pairs (Maybe String, List1 Test)
||| where the Maybe String is the tag name (Nothing for tests with no tags) and the List1 contains tests with that tag.
||| Tests with multiple tags appear in multiple groups. Tests with no tags are grouped under Nothing.
byTag : List Test -> List (Maybe String, List1 Test)
byTag = let
  -- Expand tests: for each test with tags, create (tag, test) pairs; for tests with no tags, create (Nothing, test)
  expand : Test -> List (Maybe String, Test)
  expand t = case t.tags of
    [] => [(Nothing, t)]
    tags => map (\tag => (Just tag, t)) tags
  -- Group by tag name and sort
  withName : List1 (Maybe String, Test) -> (Maybe String, List1 Test)
  withName xs@((tag, _):::_) = (tag, map snd xs)
  in map withName . groupBy ((==) `on` fst) . sortBy (compare `on` fst) . concatMap expand

export
||| Return True when a Test has no requirements and is ready to run.
isReady : Test -> Bool
isReady = null . require
