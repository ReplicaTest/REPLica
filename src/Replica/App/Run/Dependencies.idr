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
  name : Maybe String
  now : List Test
  later : List Test
  skipped : List (String, Test)

export
emptySuite : SuitePlan -> Bool
emptySuite (SPlan _ [] [] []) = True
emptySuite _ = False

public export
record TestPlan where
  constructor Plan
  ready : List SuitePlan
  waitingOthers : List SuitePlan

export
displaySuitePlan : SuitePlan -> String
displaySuitePlan x = removeTrailingNL $ unlines
  [ "  Plan:"
  , "    Now: \{show $ map name x.now}"
  , "    Later: \{show $ map name x.later}"
  , "    Skipped: \{show $ map (name . snd) x.skipped}"
  ]

export
displayPlan : TestPlan -> String
displayPlan plan = unlines $ ("Ready:"
  :: (plan.ready >>= go))
  ++ ("Postponed:"
  :: (plan.waitingOthers >>= go))
  where
    go : SuitePlan -> List String
    go p = ["\{fromMaybe "No suite" p.name}:", displaySuitePlan p]


public export
isReady : Test -> Bool
isReady = null . require

dependsOnOther : List Test -> Bool
dependsOnOther xs = any (not . flip elem (name <$> xs))  $ xs >>= require

buildSuitePlan : List1 Test -> SuitePlan
buildSuitePlan (x:::xs) = let
  (now, later) = partition (null . require) $ x::xs
  in SPlan x.suite now later []

removeRequirements : List String -> Test -> Test
removeRequirements xs y
  = {require $= filter (not . (`elem` xs))} y

export
buildPlan : (available : List Test) -> (rejected : List Test) -> TestPlan
buildPlan available rejected = uncurry Plan $
  partition (not . dependsOnOther . later) $ namedSuitePlan
  where
    cleantAvailable : List Test
    cleantAvailable
      = removeRequirements (name <$> rejected) <$> available
    suites : List (List1 Test)
    suites = groupBy ((==) `on` suite) $ sortBy (compare `on` suite) cleantAvailable
    namedSuitePlan : List SuitePlan
    namedSuitePlan = map buildSuitePlan suites

export
sortResults : List (Test, Either TestError TestResult) -> (List String, List String)
sortResults
  = bimap (map (Test.name . fst)) (map (Test.name . fst))
    . partition (either (const False) isSuccess . snd)

export
updateSuite : (successes, other : List String) -> SuitePlan -> SuitePlan
updateSuite successes other (SPlan n now later skipped) = let
  (now', later') = partition (null . require) $ removeRequirements successes <$> later
  (later'', skipped') = partition (not . any (`elem` other) . require) later'
  in SPlan n (now ++ now') later'' (skipped ++ (map (\t => (fromMaybe "unknown_test" $ head' t.require , t)) skipped'))

export
updateOnBatchResults : List (Test, Either TestError TestResult) -> TestPlan -> TestPlan
updateOnBatchResults xs plan = let
  (successes, other) = sortResults xs
  ready' = updateSuite successes other <$> plan.ready
  waiting' = updateSuite successes other <$> plan.waitingOthers
  (ready'', waitingOthers') =
    partition (not . dependsOnOther . later) waiting'
  in Plan (ready' ++ ready'') waitingOthers'
