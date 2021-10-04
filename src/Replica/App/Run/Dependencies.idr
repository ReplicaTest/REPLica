module Replica.App.Run.Dependencies

import Data.Maybe
import Data.List
import Data.List1
import Data.String

import Replica.Core.Types
import Replica.Other.String

%default total

public export
record SuitePlan where
  constructor SPlan
  now : List Test
  later : List Test
  skipped : List (String, Test)

public export
record TestPlan where
  constructor Plan
  ready : List (Maybe String, SuitePlan)
  waitingOthers : List (Maybe String, SuitePlan)

export
displayTestPlan : SuitePlan -> String
displayTestPlan x = removeTrailingNL $ unlines
  [ "  Plan:"
  , "    Now: \{show $ map name x.now}"
  , "    Later: \{show $ map name x.later}"
  , "    Skipped: \{show $ map (name . snd) x.skipped}"
  ]

export
displayPlan : TestPlan -> String
displayPlan plan = unlines $ ("Ready:"
  :: (plan.ready >>= uncurry go))
  ++ ("Postponed:"
  :: (plan.waitingOthers >>= uncurry go))
  where
    go : Maybe String -> SuitePlan -> List String
    go name p = ["\{fromMaybe "No suite" name}:", displayTestPlan p]


public export
isReady : Test -> Bool
isReady = force . null . require

dependsOnOther : List Test -> Bool
dependsOnOther xs = any (not . flip elem (name <$> xs))  $ xs >>= require

buildSuitePlan : List Test -> SuitePlan
buildSuitePlan xs = let
  (now, later) = partition (force . null . require) xs
  in SPlan now later []

removeRequirements : List String -> Test -> Test
removeRequirements xs y
  = record {require $= filter (not . (`elem` xs))} y

export
buildPlan : (available : List Test) -> (rejected : List Test) -> TestPlan
buildPlan available rejected = uncurry Plan $
  partition (not . dependsOnOther . later . snd) namedSuitePlan
  where
    cleantAvailable : List Test
    cleantAvailable
      = removeRequirements (name <$> rejected) <$> available
    suites : List (List1 Test)
    suites = groupBy ((==) `on` suite) $ sortBy (compare `on` suite) cleantAvailable
    namedSuites : List (Maybe String, List Test)
    namedSuites = map (\(t:::ts) => (t.suite, t::ts)) suites
    namedSuitePlan : List (Maybe String, SuitePlan)
    namedSuitePlan = map (map buildSuitePlan) namedSuites

updateSuite : (successes, other : List String) -> SuitePlan -> SuitePlan
updateSuite successes other (SPlan now later skipped) = let
  (later', skipped') = partition (not . (`elem` other) . name) later
  (now', later'') = partition (force . null . require) $ removeRequirements successes <$> later'
  in SPlan (now ++ now') later'' (skipped ++ (map (\t => (t.name , t)) skipped'))

export
updateOnBatchResults : List (Test, Either TestError TestResult) -> TestPlan -> TestPlan
updateOnBatchResults xs plan = let
  (successes, other) = bimap (map (name . fst)) (map (name . fst))
                     $ partition (either (const False) isSuccess . snd) $ xs
  ready' = map (updateSuite successes other) <$> plan.ready
  waiting' = map (updateSuite successes other) <$> plan.waitingOthers
  (ready'', waitingOthers') =
    partition (not . dependsOnOther . later . snd) waiting'
  in Plan (ready' ++ ready'') waitingOthers'
