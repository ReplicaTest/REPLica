||| Applitation of a `replica run` command
module Replica.App.Run

import Control.App
import Control.App.Console

import Data.Either
import Data.List
import Data.String

import Language.JSON

import System.Future
import System.Path

import Replica.App.Clock
import Replica.App.FileSystem
import Replica.App.Format
import Replica.App.Log
import Replica.App.Replica
import Replica.App.Run.Dependencies
import Replica.App.Run.Display
import Replica.App.Run.RunOne
import public Replica.App.Run.Types
import Replica.App.System

import Replica.Command.Run
import Replica.Core.Parse
import Replica.Core.Types
import Replica.Option.Global
import Replica.Other.Decorated
import Replica.Other.String
import Replica.Other.Validation

%default total

-- Create the folders needed by Replica (usually ./.replica/test and ./.replica/log)
prepareReplicaDir : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => App e String
prepareReplicaDir = do
  debug $ "GlobalConfig: \{!(show <$> get GlobalConfig)}"
  catchNew setAbsoluteReplicaDir
    (\err : FSError => throw $ CantAccessTestFile "current directory")
  rDir <- getReplicaDir
  log "Replica directory: \{rDir}"
  debug "Creating test directory: \{testDir rDir}"
  catchNew (system "mkdir -p \{show (testDir rDir)}")
    (\err : SystemError => throw $ CantAccessTestFile "\{show (testDir rDir)}")
  debug "Creating log directory: \{testDir rDir}"
  catchNew (system "mkdir -p \{show (logDir rDir)}")
    (\err : SystemError => throw $ CantAccessTestFile "\{show (logDir rDir)}")
  Just gd <- goldenDir <$> get GlobalConfig
    | Nothing => pure rDir
  debug "Creating golden-value directory: \{gd}"
  catchNew (system "mkdir -p \{show gd}")
    (\err : SystemError => throw $ CantAccessTestFile "\{show gd}")
  pure rDir

data SuiteProcess a = None | Partial a | Total a


-- add result to an existing suite or create a new one
mergeResults : List (Maybe String, List (Test, Either TestError TestResult)) ->
               (Maybe String, List (Test, Either TestError TestResult)) ->
               List (Maybe String, List (Test, Either TestError TestResult))
mergeResults [] x = [x]
mergeResults (y :: xs) x@(suiteName, results)
  = if fst y == suiteName
       then (map (++ results) y) :: xs
       else y :: mergeResults xs x

selectNextSuite : TestPlan -> SuiteProcess (SuitePlan, TestPlan)
selectNextSuite (Plan (x :: xs) waitingOthers) = Total (x, Plan xs waitingOthers)
selectNextSuite (Plan [] waitingOthers)
  = case sortBy (flip compare `on` (length . now)) waitingOthers of
         {- if all tests have at least one test waiting for another one to
            succeed,
            take the suite that has the more tests ready for execution
         -}
         [] => None
         (w::ws) => Partial (w, Plan [] ws)

prepareBatch : Nat -> SuitePlan -> (List Test, SuitePlan)
prepareBatch 0 plan = (plan.now, {now := []} plan)
prepareBatch n plan = map (\remains => {now := remains} plan) $ splitAt n plan.now

runAllTests : SystemIO (SystemError :: TestError :: e) =>
  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: TestError :: e) =>
  SystemClock (TestError :: e) =>
  Console (TestError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Console
      ] e =>  TestPlan -> App e (List (Maybe String, List (Test, Either TestError TestResult)))
runAllTests plan = do
  putStrLn $ separator 80
  putStrLn $ !bold "Running tests...\n"
  batchTests [] plan
  where

    runTotalSuite :
      List (Test, Either TestError TestResult) -> SuitePlan ->
      App e (List (Test, Either TestError TestResult))
    runTotalSuite acc plan = do
      n <- threads <$> get RunContext
      case prepareBatch n plan of
           ([], later) => do
             let errs = join [ map (\t => (t, Left Inaccessible)) plan.later
                             , map (\(reason, t) => (t, Left $ RequirementsFailed reason)) plan.skipped
                             ]
             when (not !(interactive <$> get RunContext))
               (traverse_ (\(t, r) => new t (testOutput r)) errs)
             pure $ acc ++ errs
           (now, nextBatches) => do
             debug $ withOffset 4 "Now: \{show $ length now}"
             debug $ withOffset 4 "Later: \{show $ nextBatches.now ++ nextBatches.later}"
             res <- map await <$> traverse (map (fork . delay) . processTest) now
             when (not !(interactive <$> get RunContext))
               (traverse_ (\(t, r) => new t (testOutput r)) res)
             p <- punitive <$> get RunContext
             if p && any (not . isFullSuccess . snd) res
                then pure res
                else do
                   let (suc, fai) = sortResults res
                   let newPlan = updateSuite suc fai nextBatches
                   debug $ displaySuitePlan newPlan
                   runTotalSuite (acc ++ res) $ assert_smaller plan newPlan

    runPartialSuite :
      (List (Test, Either TestError TestResult)) -> SuitePlan ->
      App e (Maybe SuitePlan, List (Test, Either TestError TestResult))
    runPartialSuite acc plan = do
      n <- threads <$> get RunContext
      case prepareBatch n plan of
           ([], later) => case acc of
             [] => do
               let errs = join [ map (\t => (t, Left Inaccessible)) plan.later
                               , map (\(reason, t) => (t, Left $ RequirementsFailed reason)) plan.skipped
                               ]
               when (not !(interactive <$> get RunContext))
                 (traverse_ (\(t, r) => new t (testOutput r)) errs)
               pure (Nothing, acc ++ errs)
             _ => pure (guard (not $ emptySuite plan) $> plan, acc)
           (now, nextBatches) => do
             debug $ withOffset 4 "Now: \{show $ length now}"
             debug $ withOffset 4 "Later: \{show $ nextBatches.now ++ nextBatches.later}"
             res <- map await <$> traverse (map (fork . delay) . processTest) now
             when (not !(interactive <$> get RunContext))
               (traverse_ (\(t, r) => new t (testOutput r)) res)
             p <- punitive <$> get RunContext
             if p && any (not . isFullSuccess . snd) res
                then pure (Nothing, res)
                else do
                   let (suc, fai) = sortResults res
                   let newPlan = updateSuite suc fai nextBatches
                   debug $ displaySuitePlan newPlan
                   runPartialSuite (acc ++ res) $ assert_smaller plan newPlan

    batchTests : List (Maybe String, List (Test, Either TestError TestResult)) ->
                 TestPlan -> App e (List (Maybe String, List (Test, Either TestError TestResult)))
    batchTests acc plan = do
      debug $ withOffset 4 $ "Run a batch"
      p <- punitive <$> get RunContext
      case selectNextSuite plan of
           None => pure acc
           Total (suite, plan') => do
             displaySuite suite.name
             suiteResults <- runTotalSuite [] suite
             let acc' = mergeResults acc (suite.name, suiteResults)
             if p && any (not . isFullSuccess . snd) suiteResults
                then pure acc'
                else
                  batchTests acc' $ assert_smaller plan $ updateOnBatchResults suiteResults plan'
           Partial (suite, plan') => do
             displaySuite suite.name
             (stuckPlan, suiteResults) <- runPartialSuite [] suite
             let acc' = mergeResults acc (suite.name, suiteResults)
             if p && any (not . isFullSuccess . snd) suiteResults
                then pure acc'
                else do
                  let plan'' = {waitingOthers $= maybe id (::) stuckPlan} plan'
                  batchTests acc' $ assert_smaller plan $ updateOnBatchResults suiteResults plan''

filterTests : FileSystem (FSError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => (s, r : List Test) -> App e TestPlan
filterTests s r = do
  activeFilters <- filter <$> get RunContext
  debug $ "Filters: \{show activeFilters}"
  let (selected, rejected) = partition (keepTest activeFilters) s
  pure $ buildPlan selected (rejected ++ r)

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

defineActiveTests : FileSystem (FSError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => App e TestPlan
defineActiveTests = do
  last <- if !((.filter.lastFailures) <$> get RunContext)
        then getLastFailures
        else do
          repl <- getReplica
          pure (repl.tests, [])
  uncurry filterTests last

extractReport : (Maybe String, List (Test, Either TestError TestResult)) ->
                List (String, Either TestError TestResult)
extractReport = map (mapFst name) . snd

export
suiteOutput : SystemIO (SystemError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Console
      ] e =>
  Maybe String -> List (Test, Either TestError TestResult) -> App e ()
suiteOutput suite tests = do
  displaySuite suite
  traverse_ (uncurry (\t, r => new t (testOutput r))) tests

export
runReplica : SystemIO (SystemError :: TestError :: e) =>
  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: TestError :: e) =>
  FileSystem (FSError :: e) =>
  SystemClock (TestError :: e) =>
  Console (TestError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => App e Stats
runReplica = do
  debug $ "Run: \{show !(get RunContext)}"
  rDir <- prepareReplicaDir
  plan <- defineActiveTests
  log $ displayPlan plan
  result <- runAllTests plan
  let logFile = lastRunLog rDir
  catchNew (writeFile logFile (show $ reportToJSON $ extractReport =<< result))
    (\err : FSError => throw $ CantAccessTestFile logFile)
  when !(interactive <$> get RunContext)
    (do putStrLn $ separator 80
        putStrLn $ !bold "Test results:"
        traverse_ (uncurry (\t, r => suiteOutput t r)) result)
  let stats = asStats $ map snd $ snd =<< result
  report stats
  pure stats
