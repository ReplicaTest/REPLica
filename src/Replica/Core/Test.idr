||| Some useful methods for test manipulation
module Replica.Core.Test

import Data.List
import Data.List1

import Replica.Core.Types

export
bySuite : List Test -> List (Maybe String, List1 Test)
bySuite = let
  withName : List1 Test -> (Maybe String, List1 Test)
  withName xs@(x:::_) = (x.suite, xs)
  in map withName . groupBy ((==) `on` suite) . sortBy (compare `on` suite)

export
isReady : Test -> Bool
isReady = null . require
