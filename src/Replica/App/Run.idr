||| Application of a `replica run` command
module Replica.App.Run

import Control.ANSI
import Control.App
import Control.App.Console

import Data.Either
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.String.Extra

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
import Replica.Other.Decorated
import Replica.Other.String
import Replica.Other.Validation

%default total

data RunContext : Type where

record TestOutput where
  constructor MkTestOutput
  status : Int
  parts : List (Part, String)

normalize : String -> String
normalize = removeTrailingNL . unlines . map unwords . filter (not . force . null) . map (assert_total words) . lines


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

-- helper for the execution of the pre/post commands
runAll :
  SystemIO (SystemError :: e) =>
  State GlobalConfig Global e =>
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

-- native way to display expectations
expectedVsGiven : State GlobalConfig Global e =>
  Nat -> String -> String -> App e (List String)
expectedVsGiven k expected given = pure $ map (withOffset k) $
  ( "Expected:" :: map !red (lines expected)) ++
  ( "Given:" :: map !green (lines given))

nativeShow : State GlobalConfig Global e =>
  Console e => Nat -> String -> String -> App e ()
nativeShow n expected given =
  traverse_ putStrLn !(expectedVsGiven n expected given)

-- Provide different ways to show the difference between expectations and givens
showDiff : SystemIO (SystemError :: e) =>
  State GlobalConfig Global e =>
  State CurrentTest Test e =>
  Console e => DiffCommand -> Nat -> String -> String -> App e ()
showDiff None n expected given = pure ()
showDiff Native n expected given = nativeShow n expected given
showDiff Diff n x y = catchNew
  (system $ "diff --minimal \{!getExpectedOutput} \{!getOutputFile}")
  (\err : SystemError => pure ())
showDiff GitDiff n x y = catchNew
  (system $ "git diff --minimal --word-diff=color --no-index -- \{!getExpectedOutput} \{!getOutputFile}")
  (\err : SystemError => pure ())
showDiff (Custom z) n x y = catchNew
  (system $ "\{z} \{!getExpectedOutput} \{!getOutputFile}")
  (\err : SystemError => pure ())

getExpectationFile : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => Part -> App e String
getExpectationFile StdOut = getExpectedOutput
getExpectationFile StdErr = getExpectedError
getExpectationFile (FileName x) = getExpectedFile x

-- on mismatch, check if we should replace the golden value
interactiveGolden : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , Exception TestError
      , Console
      ] e =>
  (source : Part) ->
  (given : String) ->
  (expected : Maybe String) ->
  App e (Maybe FailReason)
interactiveGolden source given expected = do
  t <- get CurrentTest
  putStrLn $ "\{t.name}: Golden value mismatch for \{!bold (displaySource source)}"
  showExpectedAndGiven
  putStrLn $ "Do you want to \{maybe "set" (const "replace") expected} the golden value? [N/y]"
  if !readAnswer
     then do
       f <- getExpectationFile source
       log $ "Creating expectation file \{f}"
       handle (writeFile f given)
         (const $ pure Nothing)
         (\err : FSError => throw $ FileSystemError "Cannot write golden value in: \{f}")
     else pure . Just $ maybe
       (WrongOutput source given $ singleton (Generated ** Nothing))
       (\x => WrongOutput source given $ singleton (Generated ** Just x))
       expected
  where

    readAnswer : App e Bool
    readAnswer = do
      answer <- getLine
      pure $ toLower answer `elem` ["y", "yes"]

    showExpectedAndGiven : App e ()
    showExpectedAndGiven = do
      let Just str = expected
        | _ => do
          putStrLn "Expected: Nothing Found"
          putStrLn "Given:"
          putStrLn given
      let d = case !(diff <$> get GlobalConfig) of
                   None => Native
                   d' => d'
      showDiff d 0 str given

getExpected : FileSystem (FSError :: e) =>
  Has [ State RunContext RunCommand
      , Exception TestError
      , Console
      ] e => String -> App e (Maybe String)
getExpected src= do
  handle (readFile src)
    (pure . Just)
    (\err : FSError => case err of
        MissingFile _ => pure Nothing
        err => throw $ FileSystemError "Cannot read expectation")

checkConsecutive : (xs : List String) -> (given : String) -> Maybe String
checkConsecutive [] given = Nothing
checkConsecutive (x :: xs) given = do
  let (s::_) = unpack x
      | [] => checkConsecutive xs given
  case break (== s) given of
       (_, "") => Just x
       (_, str) => if isPrefixOf x str
                      then checkConsecutive xs (assert_smaller given $ drop (length x) str)
                      else checkConsecutive (x::xs) (assert_smaller given $ drop 1 str)

checkContains : List String -> String -> Maybe (List1 String)
checkContains xs given =
  fromList $ catMaybes $ map (\exp => guard (not $ isInfixOf exp given) $> exp) xs

getPartContent : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console ] e =>
  (part: Part) ->
  App e String
getPartContent StdOut =
  catchNew (readFile !getOutputFile)
    (\err : FSError => throw $ FileSystemError "Standard output cannot be read")
getPartContent StdErr =
  catchNew (readFile !getErrorFile)
    (\err : FSError => throw $ FileSystemError "Standard error cannot be read")
getPartContent (FileName x) =
  catchNew (readFile !(getWatchedFile x))
  (\err : FSError => throw $ FileSystemError "Expected file \{show x} cannot be read")

getPartExpectation : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console ] e =>
  (part: Part) ->
  App e (Maybe String)
getPartExpectation part =
  catchNew (Just <$> readFile !(getExpectationFile part)) (\err : FSError => pure Nothing)

checkContent : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , Exception TestError
      , Console
      ] e =>
  (spaceSensitive :Bool) ->
  (given : String) ->
  (expected : Maybe String) ->
  Expectation ->
  Maybe (e : Expectation ** ExpectationError e)
checkContent spaceSensitive given _ exp@(Exact expected) = let
  e : String = if spaceSensitive then expected else normalize expected
  g : String = if spaceSensitive then given else normalize given
  in guard (e /= g) $> (exp ** ())
checkContent spaceSensitive given _ exp@(StartsWith expected) = let
  e : String = if spaceSensitive then expected else normalize expected
  g : String = if spaceSensitive then given else normalize given
  in guard (not $ e `isPrefixOf` g) $> (exp ** ())
checkContent spaceSensitive given _ exp@(EndsWith expected) = let
  e : String = if spaceSensitive then expected else normalize expected
  g : String = if spaceSensitive then given else normalize given
  in guard (not $ e `isSuffixOf` g) $> (exp ** ())
checkContent spaceSensitive given _ exp@(Partial Ordered xs) = let
  es = if spaceSensitive then xs else map normalize xs
  g : String = if spaceSensitive then given else normalize given
  in MkDPair exp <$> checkConsecutive es given
checkContent spaceSensitive given _ exp@(Partial Whatever xs) = let
  es = if spaceSensitive then xs else map normalize xs
  g : String = if spaceSensitive then given else normalize given
  in MkDPair exp <$> checkContains es given
checkContent spaceSensitive given Nothing Generated =
  Just (Generated ** Nothing)
checkContent spaceSensitive given (Just expected) Generated = let
  e : String = if spaceSensitive then expected else normalize expected
  g : String = if spaceSensitive then given else normalize given
  in guard (e /= g) $> (Generated ** Just expected)

checkOutput : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console ] e =>
  (part: Part) ->
  (expectations : List Expectation) ->
  App e (Maybe FailReason)
checkOutput part expectations = do
  log $ withOffset 4 $ "Check \{displaySource part}"
  debug $ withOffset 4 $ show expectations
  t <- get CurrentTest
  given <- getPartContent part
  golden <- getPartExpectation part
  let result = fromList $ catMaybes $
    checkContent {e} t.spaceSensitive given golden <$> expectations
  debug $ withOffset 4 $ "Errors: \{show $ map (map fst) result}"
  pure $ map (WrongOutput part given) result

checkStatus : Maybe (Either Bool Nat) -> Nat -> Maybe FailReason
checkStatus Nothing y = Nothing
checkStatus (Just (Left True)) y = guard (y /= 0) $> WrongStatus y (Left True)
checkStatus (Just (Left False)) y = guard (y == 0) $> WrongStatus y (Left False)
checkStatus (Just (Right x)) y = guard (y /= x) $> WrongStatus y (Right x)

checkExpectations :  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console ] e =>
  (exitCode : Nat) ->
  App e TestResult
checkExpectations exitCode = do
  log $ withOffset 2 "Checking expectations"
  t <- get CurrentTest
  ctx <- get RunContext
  let statusCheck = checkStatus t.status exitCode
  expResults <- traverse (uncurry checkOutput) t.expectations
  let failures = maybe id (::) statusCheck $ catMaybes expResults
  debug $ withOffset 4 "Check success"
  let (x :: xs) = failures
    | _ => pure Success
  let Nothing = statusCheck
    | _ => pure $ Fail failures
  debug $ withOffset 4
        "Check interactive: \{show $ isNothing statusCheck} \{show ctx.interactive}"
  let (Nothing, True) = (statusCheck, ctx.interactive)
    | _ =>  pure $ Fail failures
  [] <- map catMaybes $ traverse askForGolden failures
    | xs => pure $ Fail xs
  pure Success
  where
    askForGolden : FailReason -> App e (Maybe FailReason)
    askForGolden (WrongOutput x given ((Generated ** expected) ::: [])) =
      interactiveGolden x given expected
    askForGolden x = pure $ Just x

-- if an input is given for a test, write it in a file to pass it to the command
generateInput : FileSystem (FSError :: e) =>
      Has [ State CurrentTest Test
          , State GlobalConfig Global
          , Exception TestError ] e =>
      App e (Maybe String)
generateInput = do
  Just input <- input <$> get CurrentTest
    | _ => pure Nothing
  f <- getInputFile
  catchNew (writeFile f input)
    (\e : FSError => throw $ FileSystemError "Can't write input file \{f}")
  pure (Just f)

-- run the tested commands and gather the outputs (standard output and file output, if provided)
generateOutputs : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console
      ] e => App e (Either TestResult Nat)
generateOutputs = do
  t <- get CurrentTest
  debug $ withOffset 2 "Check pending"
  let False = t.pending
    | True => pure $ Left Skipped
  statusFile <- getStatusFile
  ignore $ runCommand !getOutputFile !getErrorFile statusFile
  Right . fromMaybe 0 . parsePositive <$>
    catchNew (readFile statusFile) (\err : FSError => pure "0")
  where
    runCommand : (outputFile, errorFile, statusFile : String) -> App e Int
    runCommand outputFile errorFile statusFile = do
      t <- get CurrentTest
      inputFile <- generateInput
      let cmd = """
                (\{t.command}) \{maybe "" ("< " ++ )inputFile} 1> \"\{outputFile}\" 2> \"\{errorFile}\";
                EXIT_STATUS=$?;
                echo $EXIT_STATUS > \{statusFile};
                exit $EXIT_STATUS
                """
      log $ withOffset 2 "Running command: \{cmd}"
      handle (system cmd) (const $ pure 0) (\(Err n) => pure n)

testCore : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console
      ] e => App e TestResult
testCore = do
  Right exitCode <- generateOutputs
    | Left res => pure res
  checkExpectations exitCode

-- the whole test execution, including pre and post operation
performTest : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console
      ] e => App e TestResult
performTest = do
  t <- get CurrentTest
  runAll (Just "Before") InitializationFailed t.beforeTest
  res <- testCore
  runAll (Just "After") (WrapUpFailed res) t.afterTest
  pure res

-- move to the right directory and perform the test
runTest : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State RunContext RunCommand
      , State GlobalConfig Global
      , Exception TestError
      , Console
      ] e => App e TestResult
runTest = do
  ctx <- get RunContext
  t <- get CurrentTest
  createTestDir
  createGoldenDir
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
    continueIfExists : FSError -> App e ()
    continueIfExists (FileExists _) = pure ()
    continueIfExists _ = throw $ FileSystemError "Cant't create or access test directory"
    createTestDir : App e ()
    createTestDir = do
      catchNew (createDir !getSingleTestDir) continueIfExists
      catchNew (createDir !getSingleTestFileDir) continueIfExists
    createGoldenDir : App e ()
    createGoldenDir = do
      catchNew (createDir !getSingleTestGoldenDir) continueIfExists
      catchNew (createDir !getSingleTestGoldenFileDir) continueIfExists

displaySuite :
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Console
      ] e => Maybe String -> App e ()
displaySuite suite = putStrLn $ !bold $ maybe "No suite given:" ("Suite: " <+>) suite

testOutput : SystemIO (SystemError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , State CurrentTest Test
      , Console
      ] e => Either TestError TestResult -> App e ()
testOutput (Left y) = do
  t <- get CurrentTest
  putStr (withOffset 2 $ (!yellow "\{!err} \{t.name}: "))
  putStrLn (displayTestError y)
testOutput (Right Skipped) = do
  t <- get CurrentTest
  putStrLn $ withOffset 2 "\{!pending} \{t.name}"
testOutput (Right Success) = do
  t <- get CurrentTest
  if !(hideSuccess <$> get RunContext)
     then pure ()
     else putStrLn $ withOffset 2 "\{!ok} \{t.name}"
testOutput (Right (Fail xs)) = do
  t <- get CurrentTest
  putStrLn $ withOffset 2 $ !red "\{!ko} \{t.name}:"
  traverse_ (putStrLn . withOffset 6 . !red) (xs >>= displayFailReason)
  traverse_ writeFailure xs
  where
    multilineDisplay : (offset : Nat) -> (content : String) -> App e ()
    multilineDisplay offset = traverse_ (putStrLn . withOffset offset) . lines

    displayError : (given : String)  -> (exp : (e: Expectation ** ExpectationError e)) -> App e ()
    displayError given (MkDPair (Exact x) snd) =
      let content = case !(diff <$> get GlobalConfig) of
            None => multilineDisplay 8 x
            Native => multilineDisplay 8 x
            d' => showDiff d' 8 given x
      in putStrLn (withOffset 6 "Exact expectation mismatch:") >> content
    displayError given (MkDPair (StartsWith x) snd) =
      putStrLn (withOffset 6 "Start mismatch:") >> multilineDisplay 4 x
    displayError given (MkDPair (EndsWith x) snd) =
      putStrLn (withOffset 6 "End mismatch:") >> multilineDisplay 4 x
    displayError given (MkDPair (Partial Ordered ys) snd) = do
      putStrLn (withOffset 6 "Consecutive expectations mismatch, first not found:")
      multilineDisplay 8 snd
    displayError given (MkDPair (Partial Whatever ys) snd) = do
      putStrLn (withOffset 6 "Contains expectations mismatch, not found:")
      traverse_ (multilineDisplay 8) snd
    displayError given (MkDPair Generated Nothing) = pure ()
    displayError given (MkDPair Generated (Just x)) =
      let content = case !(diff <$> get GlobalConfig) of
            None => multilineDisplay 8 x
            Native => multilineDisplay 8 x
            d' => showDiff d' 8 given x
      in putStrLn (withOffset 6 "Golden value expectation mismatch:") >> content

    writeFailure : FailReason -> App e ()
    writeFailure (WrongStatus _ expected) = pure ()
    writeFailure (ExpectedFileNotFound x) = pure ()
    writeFailure (WrongOutput x given ys) = do
      putStrLn $ withOffset 6 $ !bold "Error on \{displaySource x}:"
      traverse_ (displayError given) ys
      putStrLn $ withOffset 6 $ "Given:"
      traverse_ (putStrLn . withOffset 8) $ lines given

data SuiteProcess a = None | Partial a | Total a

runAllTests : SystemIO (SystemError :: TestError :: e) =>
  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: TestError :: e) =>
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
    selectNextSuite : TestPlan -> SuiteProcess (SuitePlan, TestPlan)
    selectNextSuite (Plan (x :: xs) waitingOthers) = Total (x, Plan xs waitingOthers)
    selectNextSuite (Plan [] waitingOthers)
      = case sortBy (compare `on` (length . now)) waitingOthers of
             [] => None
             (w::ws) => Partial (w, Plan [] ws)

    prepareBatch : Nat -> SuitePlan -> (List Test, SuitePlan)
    prepareBatch n plan = if n == 0
                             then (plan.now, record {now = []} plan)
                             else map (\n' => record {now = n'} plan) $ splitAt n plan.now

    processTest : Test -> App e (Test, Either TestError TestResult)
    processTest x = do
      let False = x.pending
        | True => pure (x, Right Skipped)
      r <- handle
             (new x runTest)
             (pure . MkPair x . Right)
             (\err : TestError => pure (x, Left err))
      pure r

    processResult : SuitePlan -> TestPlan -> List (Test, Either TestError TestResult) ->
                    Either TestPlan (SuitePlan, TestPlan)
    processResult (SPlan _ [] [] skipped) y xs = Left (updateOnBatchResults xs y)
    processResult x y xs = case updateOnBatchResults xs (record {ready $= (x::)} y) of
                                Plan (x'::now) later => Right (x', Plan now later)
                                p => Left p

    runTotalSuite : List (Test, Either TestError TestResult) -> SuitePlan ->
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

    runPartialSuite : (List (Test, Either TestError TestResult)) -> SuitePlan ->
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
             _ => pure (Just plan, acc)
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

    mergeResults : List (Maybe String, List (Test, Either TestError TestResult)) ->
                   (Maybe String, List (Test, Either TestError TestResult)) ->
                   List (Maybe String, List (Test, Either TestError TestResult))
    mergeResults [] x = [x]
    mergeResults (y :: xs) x = if fst y == fst x
                                  then (map (++ snd x) y) :: xs
                                  else y :: mergeResults xs x


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
                  let plan'' = record {waitingOthers $= maybe id (::) stuckPlan} plan'
                  batchTests acc' $ assert_smaller plan $ updateOnBatchResults suiteResults plan''

report : Console e => State GlobalConfig Global e => Stats -> App e ()
report x = do
  putStrLn $ separator 80
  putStrLn $ !bold "Summary:"
  let nb = countTests x
  if nb == 0
     then putStrLn $ withOffset 2 "No test"
     else putStrLn $ removeTrailingNL $ unlines $ catMaybes
    [ guard (x.successes > 0) $>
        withOffset 2 "\{!ok} (Success): \{show x.successes} / \{show nb}"
    , guard (x.failures > 0) $>
        withOffset 2 "\{!ko} (Failure): \{show x.failures} / \{show nb}"
    , guard (x.errors > 0) $>
        withOffset 2 "\{!err}  (Errors): \{show x.errors} / \{show nb}"
    , guard (x.skipped > 0) $>
        withOffset 2 "\{!pending}  (Pending): \{show x.skipped} / \{show nb}"
    ]


filterTests : FileSystem (FSError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Exception ReplicaError
      , Console
      ] e => (s, r : List Test) -> App e TestPlan
filterTests s r = do
  f <- filter <$> get RunContext
  debug $ "Filters: \{show f}"
  let (selected, rejected) = partition (keepTest f) s
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
