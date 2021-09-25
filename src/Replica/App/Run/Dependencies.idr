module Replica.App.Run.Dependencies

import Data.List
import Data.String

import Replica.Core.Types
import Replica.Other.String

%default total

public export
record TestPlan where
  constructor Plan
  now : List Test
  later : List Test
  skipped : List (String, Test)

export
displayPlan : TestPlan -> String
displayPlan x = removeTrailingNL $ unlines
  [ "Plan:"
  , "  Now: \{show $ map name x.now}"
  , "  Later: \{show $ map name x.later}"
  , "  Skipped: \{show $ map (name . snd) x.skipped}"]


public export
isReady : Test -> Bool
isReady = force . null . require

export
buildPlan : List Test -> TestPlan
buildPlan xs = let
  (now, later) = partition isReady xs
  in Plan now later []

export
validate : String -> TestPlan -> TestPlan
validate x y = let
  y' : TestPlan = record {later $= map (record {require $= filter (/= x)})} y
  (now', later') = partition isReady y'.later
  in record {now $= (++ now') . filter (not . (== x) . name), later = later'} y

export
fail : String -> TestPlan -> TestPlan
fail x y = let
  (skipped', later') = partition (elem x . require) y.later
  y' = record { now $= filter (not . (== x) . name)
              , later = later'
              , skipped $= (map (MkPair x) skipped' ++)} y
  in foldl (\tp, t => fail t.name $ assert_smaller y tp) y' skipped'
