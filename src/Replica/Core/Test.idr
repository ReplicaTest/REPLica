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
||| Return True when a Test has no requirements and is ready to run.
isReady : Test -> Bool
isReady = null . require
