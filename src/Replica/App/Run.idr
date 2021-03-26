module Replica.App.Run

import Control.ANSI
import Control.App
import Control.App.Console

import Data.List
import Data.List1
import Data.Maybe
import Data.String

import Language.JSON

import System.Future
import System.Path

import Replica.App.FileSystem
import Replica.App.Format
import Replica.App.Log
import Replica.App.Replica
import Replica.App.Run.Dependencies
import Replica.App.System

import Replica.Command.Run
import Replica.Core.Parse
import Replica.Core.Types
import Replica.Option.Global
import Replica.Other.String
import Replica.Other.Validation

%default total

data RunContext : Type where

prepareReplicaDir : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State RunContext RunAction
      , State GlobalConfig GlobalOption
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
  pure rDir

runAll :
  SystemIO (SystemError :: e) =>
  State GlobalConfig GlobalOption e =>
  Exception TestError e =>
  Console e =>
  (phase : Maybe String) ->
  (String -> TestError) ->
  List String -> App e ()
runAll phase liftError [] = pure ()
runAll phase liftError (x :: xs) = do
  maybe (pure ()) (\p => log "\{p}: \{x}") phase
  handle (system "(\{x}) 1> /dev/null 2> /dev/null")
    (const $ runAll phase liftError xs)
    (\err : SystemError => throw $ liftError x)

expectedvsGiven : State GlobalConfig GlobalOption e =>
  Nat -> String -> String -> App e (List String)
expectedvsGiven k expected given = pure $ map (withOffset k) $
  ( "Expected:" :: map !red (forget $ lines expected)) ++
  ( "Given:" :: map !green (forget $ lines given))

nativeShow : State GlobalConfig GlobalOption e =>
  Console e => String -> String -> App e ()
nativeShow expected given =
  putStrLn $ unlines !(expectedvsGiven 0 expected given)

showDiff : SystemIO (SystemError :: e) =>
  State GlobalConfig GlobalOption e =>
  State CurrentTest Test e =>
  Console e => DiffCommand -> String -> String -> App e ()
showDiff Native expected given = nativeShow expected given
showDiff Diff x y = catchNew
  (system $ "git diff --minimal --word-diff=color --no-index -- \{!getExpectedFile} \{!getOutputFile}")
  (\err : SystemError => nativeShow x y)
showDiff GitDiff x y = catchNew
  (system $ "git diff --minimal --word-diff=color --no-index -- \{!getExpectedFile} \{!getOutputFile}")
  (\err : SystemError => nativeShow x y)
showDiff (Custom z) x y = catchNew
  (system $ "\{z} \{!getExpectedFile} \{!getOutputFile}")
  (\err : SystemError => nativeShow x y)

expectedVsGiven : SystemIO (SystemError :: e) =>
  State CurrentTest Test e =>
  State GlobalConfig GlobalOption e =>
  Console e => Maybe String -> String -> App e ()
expectedVsGiven old given = do
  let Just str = old
        | Nothing => do
    putStrLn "Expected: Nothing Found"
    putStrLn "Given:"
    putStrLn given
  showDiff !(diff <$> get GlobalConfig) str given

askForNewGolden : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig GlobalOption
      , Exception TestError
      , Console
      ] e => Maybe String -> String -> App e TestResult
askForNewGolden old given = do
  t <- get CurrentTest
  putStrLn $ "\{t.name}: Golden value mismatch"
  expectedVsGiven old given
  putStrLn $ "Do you want to \{maybe "set" (const "replace") old} the golden value? [N/y]"
  if !readAnswer
     then do
       expectedFile <- catchNew getExpectedFile
          (\err : FSError => throw $ FileSystemError "Can't resolve expectation file")
       handle (writeFile expectedFile given)
         (const $ pure Success)
         (\err : FSError => throw $ FileSystemError "Cannot write golden value")
     else pure $ maybe (Fail [WrongOutput GoldenIsMissing])
                       (Fail . pure . WrongOutput . flip DifferentOutput given)
                       old
  where
    readAnswer : App e Bool
    readAnswer = do
      answer <- getLine
      pure $ toLower answer `elem` ["y", "yes"]

checkOutput :  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig GlobalOption
      , State RunContext RunAction
      , Exception TestError
      , Console ] e =>
  (mustSucceed : Maybe Bool) -> (status : Int) ->
  (expectedOutput : Maybe String) -> (output : String) ->
  App e TestResult
checkOutput mustSucceed status expectedOutput output
  = do
    ctx <- get RunContext
    let Fail err = checkExpectation
      | Success => pure $ checkStatus
    let Success = checkStatus
      | Fail err2 => pure $ Fail $ err ++ err2
    if ctx.interactive
       then askForNewGolden expectedOutput output
       else pure $ Fail err
    where
      checkStatus : TestResult
      checkStatus =
        let Just s = mustSucceed
              | Nothing => Success
        in if (s && status == 0) || (not s && status /= 0)
              then Success
              else Fail [WrongStatus s]
      checkExpectation : TestResult
      checkExpectation =
        let Just exp = expectedOutput
              | Nothing => Fail [WrongOutput GoldenIsMissing]
        in if exp == output
              then Success
              else Fail [WrongOutput $ DifferentOutput exp output]

getExpected : FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig GlobalOption
      , State RunContext RunAction
      , Exception TestError
      , Console
      ] e => String -> App e (Maybe String)
getExpected given = do
  t <- get CurrentTest
  expectedFile <- getExpectedFile
  handle (readFile expectedFile)
    (pure . Just)
    (\err : FSError => case err of
        MissingFile _ => pure Nothing
        err => throw $ FileSystemError "Cannot read expectation")

testCore : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig GlobalOption
      , State RunContext RunAction
      , Exception TestError
      , Console
      ] e => App e TestResult
testCore = do
  t <- get CurrentTest
  outputFile <- getOutputFile
  exitStatus <- handle (system $ "(\{t.command}) > \"\{outputFile}\"")
    (const $ pure 0)
    (\(Err n) => pure n)
  output <- catchNew (readFile $ outputFile)
    (\e : FSError => throw $
          FileSystemError "Can't read output file \{outputFile}")
  expected <- getExpected output
  checkOutput t.mustSucceed exitStatus expected output

performTest : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig GlobalOption
      , State RunContext RunAction
      , Exception TestError
      , Console
      ] e => App e TestResult
performTest = do
  t <- get CurrentTest
  runAll (Just "Before") InitializationFailed t.beforeTest
  log $ withOffset 2 "Running command: \{t.command}"
  res <- testCore
  runAll (Just "After") (WrapUpFailed res) t.afterTest
  pure res

runTest : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State RunContext RunAction
      , State GlobalConfig GlobalOption
      , Exception TestError
      , Console
      ] e => App e TestResult
runTest = do
  ctx <- get RunContext
  t <- get CurrentTest
  let wd = fromMaybe ctx.workingDir t.workingDir
  log "Executing \{t.name}"
  debug $ withOffset 2 $ show t
  log   $ withOffset 2 "Working directory: \{show wd}"
  catchNew (inDir wd performTest)
    (\err : FSError => throw $ FileSystemError
      "Error: cannot enter or exit test working directory \{show wd}")
  where
    inDir : (dir : String) -> App e a -> App (FSError :: e) a
    inDir dir exec = do
      pwd <- getCurrentDir
      changeDir dir
      Right res <- lift $ catch (Right <$> exec)
                                (\err : TestError => pure $ Left err)
        | Left err => changeDir pwd >> lift (throw err)
      changeDir pwd
      pure res

testOutput :
  Has [ State RunContext RunAction
      , State GlobalConfig GlobalOption
      , Console
      ] e => String -> Either TestError TestResult -> App e ()
testOutput name (Left y) = do
  putStr (withOffset 2 $ (!yellow "\{!err} \{name}: "))
  putStrLn (displayTestError y)
testOutput name (Right Success) = do
  if !(hideSuccess <$> get RunContext)
     then pure ()
     else putStrLn $ withOffset 2 "\{!ok} \{name}"
testOutput name (Right (Fail xs)) = do
  putStrLn $ withOffset 2 $ !red "\{!ko} \{name}: \{unwords $ map displayFailReason xs}"
  let Just (expected, given) = getContentMismatch xs
    | Nothing => pure ()
  putStrLn $ unlines !(expectedvsGiven 6 expected given)
  where
    getContentMismatch : List FailReason -> Maybe (String, String)
    getContentMismatch [] = Nothing
    getContentMismatch (WrongOutput (DifferentOutput x y) :: _) = Just (x, y)
    getContentMismatch (_ :: xs) = getContentMismatch xs

runAllTests : SystemIO (SystemError :: TestError :: e) =>
  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: TestError :: e) =>
  Console (TestError :: e) =>
  Has [ State RunContext RunAction
      , State GlobalConfig GlobalOption
      , Console
      ] e =>  TestPlan -> App e (List (String, Either TestError TestResult))
runAllTests plan = do
  putStrLn $ separator 60
  putStrLn $ !bold "Running tests..."
  batchTests [] plan
  where
    processTest : Test -> App e (String, Either TestError TestResult)
    processTest x = do
      rdir <- getReplicaDir
      r <- handle
             (new x runTest)
             (pure . MkPair x.name . Right)
             (\err : TestError => pure (x.name, Left err))
      pure r
    prepareBatch : Nat -> TestPlan -> (List Test, List Test)
    prepareBatch n plan = if n == 0
                             then (plan.now, Prelude.Nil)
                             else splitAt n plan.now
    processResult : TestPlan -> (String, Either TestError TestResult) -> TestPlan
    processResult plan (tName, Right Success) = validate tName plan
    processResult plan (tName, _) = fail tName plan
    batchTests : List (String, Either TestError TestResult) ->
                 TestPlan -> App e (List (String, Either TestError TestResult))
    batchTests acc plan = do
      n <- threads <$> get RunContext
      case prepareBatch n plan of
           ([], later) => pure $ join
              [ acc
              , map (\t => (t.name, Left Inaccessible)) plan.later
              , map (\(reason, t) => (t.name, Left $ RequirementsFailed reason)) plan.skipped
              ]
           (now, nextBatches) => do
             res <- map await <$> traverse (map (fork . delay) . processTest) now
             when (not !(interactive <$> get RunContext))
               (traverse_ (uncurry testOutput) res)
             p <- punitive <$> get RunContext
             if p && any (not . isSuccess . snd) res
                then pure res
                else do
                   let plan' = record {now = nextBatches} plan
                   debug $ displayPlan plan'
                   batchTests (acc ++ res) $ assert_smaller plan (foldl processResult plan' res)

report : Console e => State GlobalConfig GlobalOption e => Stats -> App e ()
report x = do
  putStrLn $ separator 60
  putStrLn $ !bold "Summary:"
  let nb = countTests x
  if nb == 0
     then putStrLn $ withOffset 2 "No test"
     else putStrLn $ unlines $ catMaybes
    [ guard (x.successes > 0) $>
        withOffset 2 "\{!ok} (Success): \{show x.successes} / \{show nb}"
    , guard (x.failures > 0) $>
        withOffset 2 "\{!ko} (Failure): \{show x.failures} / \{show nb}"
    , guard (x.errors > 0) $>
        withOffset 2 "\{!err}  (Errors): \{show x.errors} / \{show nb}"
    ]


filterTests : FileSystem (FSError :: e) =>
  Has [ State RunContext RunAction
      , State GlobalConfig GlobalOption
      , Exception ReplicaError
      , Console
      ] e => (s, r : List Test) ->App e TestPlan
filterTests s r = do
  selectedTests <- (.filter.only) <$> get RunContext
  excludedTests <- (.filter.exclude)<$> get RunContext
  selectedTags <- (.filter.onlyTags) <$> get RunContext
  excludedTags <- (.filter.excludeTags) <$> get RunContext
  debug $ "Tags: \{show selectedTags}"
  debug $ "Names: \{show selectedTests}"
  let (selected, rejected) =
    partition (go selectedTags selectedTests excludedTags excludedTests) s
  pure $ foldl (\p, t => validate t.name p) (buildPlan selected) (r ++ rejected)
  where
    go : (tags, names, negTags, negNames : List String) -> Test -> Bool
    go tags names negTags negNames t =
      (null tags || not (null $ intersect t.tags tags))
      && (null names || (t.name `elem` names))
      && (null negTags || (null $ intersect t.tags negTags))
      && (null negNames || (not $ t.name `elem` negNames))


getLastFailures : FileSystem (FSError :: e) =>
  Has [ State RunContext RunAction
      , State GlobalConfig GlobalOption
      , Exception ReplicaError
      , Console
      ] e => App e (List Test, List Test)
getLastFailures = do
  repl <- getReplica RunContext file
  logFile <- lastRunLog <$> getReplicaDir
  lastLog <- catchNew (readFile logFile)
    (\err : FSError => throw $ CantAccessTestFile logFile)
  let Just json = parse lastLog
    | Nothing => throw $ InvalidJSON []
  let Valid report = parseReport json
    | Error err => throw $ InvalidJSON err
  let notWorking = fst <$> filter (not . isSuccess . snd) report
  let (selected, rejected) = partition (flip elem notWorking . name) repl.tests
  debug $ "Previous invalid tests: \{show selected}"
  pure (selected, rejected)

defineActiveTests : FileSystem (FSError :: e) =>
  Has [ State RunContext RunAction
      , State GlobalConfig GlobalOption
      , Exception ReplicaError
      , Console
      ] e => App e TestPlan
defineActiveTests = do
  last <- the (App e (List Test, List Test)) $
     if !((.filter.lastFailures) <$> get RunContext)
        then getLastFailures
        else do
          repl <- getReplica RunContext file
          pure (repl.tests, [])
  uncurry filterTests last

export
runReplica : SystemIO (SystemError :: TestError :: e) =>
  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: TestError :: e) =>
  FileSystem (FSError :: e) =>
  Console (TestError :: e) =>
  Has [ State RunContext RunAction
      , State GlobalConfig GlobalOption
      , Exception ReplicaError
      , Console
      ] e => App e Stats
runReplica = do
  debug $ "Command: \{show !(get RunContext)}"
  rDir <- prepareReplicaDir
  plan <- defineActiveTests
  log $ displayPlan plan
  result <- runAllTests plan
  let logFile = lastRunLog rDir
  catchNew (writeFile logFile (show $ reportToJSON result))
    (\err : FSError => throw $ CantAccessTestFile logFile)
  when !(interactive <$> get RunContext)
    (do putStrLn $ separator 60
        putStrLn $ !bold "Test results:"
        traverse_ (uncurry testOutput) result)
  let stats = asStats $ snd <$> result
  report $ stats
  pure stats
