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
  std : String
  file : Maybe String

normalize : String -> String
normalize = unlines . map unwords . filter (not . force . null) . map (assert_total words) . forget . lines

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
  ( "Expected:" :: map !red (forget $ lines expected)) ++
  ( "Given:" :: map !green (forget $ lines given))

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

-- on mismatch, check if we should replace the golden value
interactiveGolden : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , Exception TestError
      , Console
      ] e =>
  (source : Maybe String) ->
  (given : String) ->
  (expected : Maybe String) ->
  App e (Maybe FailReason)
interactiveGolden source given expected = do
  t <- get CurrentTest
  putStrLn $ "\{t.name}: Golden value mismatch for \{maybe "standard output" ("file " ++) source}"
  showExpectedAndGiven
  putStrLn $ "Do you want to \{maybe "set" (const "replace") expected} the golden value? [N/y]"
  if !readAnswer
     then do
       f <- expectedFile
       handle (writeFile f given)
         (const $ pure Nothing)
         (\err : FSError => throw $ FileSystemError "Cannot write golden value in: \{f}")
     else pure . Just $ maybe
       (WrongOutput source (GoldenIsMissing given))
       (WrongOutput source . flip DifferentOutput given)
       expected
  where

    expectedFile : App e String
    expectedFile = maybe getExpectedOutput (const getExpectedFile) source

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


-- check partial expectations
checkPartial : (ordered : OrderSensitive) -> (xs : List String) ->
               (given : String) ->
               Maybe FailReason
checkPartial Ordered [] given = Nothing
checkPartial Ordered (x :: xs) given = do
  let (s::_) = unpack x
      | [] => checkPartial Ordered xs given
  case break (== s) given of
       (_, "") => Just $ WrongOutput Nothing (PartialOutputMismatch Ordered (x::xs) given)
       (_, str) => if isPrefixOf x str
                      then checkPartial Ordered xs (assert_smaller given $ drop (length x) str)
                      else checkPartial Ordered (x::xs) (assert_smaller given $ drop 1 str)
checkPartial Whatever xs given = let
  errors = catMaybes $ map (\exp => guard (not $ isInfixOf exp given) $> exp) xs
  in case errors of
          [] => Nothing
          _  => Just $ WrongOutput Nothing (PartialOutputMismatch Whatever errors given)

checkOutput : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console ] e =>
  (source : Maybe String) ->
  (given : String) ->
  App e (Maybe (FailReason, Maybe (App e (Maybe FailReason))))
checkOutput source given = do
  t <- get CurrentTest
  let Generated = maybe t.expectation (const Generated) source
       | Exact expected => noInteractionOnFailure $
                             checkExact t.spaceSensitive expected given
       | Partial x xs => noInteractionOnFailure $ checkPartial' t.spaceSensitive x xs given
  Just expected <- getExpected $ maybe !getExpectedOutput (const !getExpectedFile) source
    | Nothing => interactiveFailure Nothing $ Just $ WrongOutput source (GoldenIsMissing given)
  interactiveFailure (Just expected) $ checkExact t.spaceSensitive expected given
  where
    checkExact : (spaceSensitive : Bool) -> (expected, given : String) -> Maybe FailReason
    checkExact spaceSensitive expected given = let
      e : String = if spaceSensitive then expected else normalize expected
      g : String = if spaceSensitive then given else normalize given
      in guard (e /= g) $> WrongOutput source (DifferentOutput expected given)
    checkPartial' : (spaceSensitive : Bool) -> (o : OrderSensitive) ->
                    (expected : List String) -> (given : String) -> Maybe FailReason
    checkPartial' spaceSensitive o expected given = let
      e : List String = if spaceSensitive then expected else map normalize expected
      g : String = if spaceSensitive then given else normalize given
      in checkPartial o e g
    noInteractionOnFailure : Maybe FailReason ->
      App e (Maybe (FailReason, Maybe (App e (Maybe FailReason))))
    noInteractionOnFailure = pure . map (flip MkPair Nothing)
    interactiveFailure :
      Maybe String ->
      Maybe FailReason ->
      App e (Maybe (FailReason, Maybe (App e (Maybe FailReason))))
    interactiveFailure expected =
      pure . map (flip MkPair (Just $ interactiveGolden source given expected))

checkStatus : Maybe Bool -> Int -> Maybe FailReason
checkStatus Nothing y = Nothing
checkStatus (Just True) y = guard (y /= 0) $> WrongStatus True
checkStatus (Just False) y = guard (y == 0) $> WrongStatus False

checkExpectations :  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console ] e =>
  (ouputs : TestOutput) ->
  App e TestResult
checkExpectations outputs = do
  t <- get CurrentTest
  ctx <- get RunContext
  let statusFailure = checkStatus t.mustSucceed outputs.status
  stdFailure <- checkOutput Nothing outputs.std
  fileFailure <- maybe (pure Nothing) (checkOutput t.file) outputs.file
  let failures = catMaybes [statusFailure, fst <$> stdFailure, fst <$> fileFailure]
  debug $ withOffset 4 "Check success"
  let (x :: xs) = failures
    | _ => pure Success
  let Nothing = statusFailure
    | _ => pure $ Fail failures
  debug $ withOffset 4
        "Check interactive: \{show $ isNothing statusFailure} \{show ctx.interactive}"
  let (Nothing, True) = (statusFailure, ctx.interactive)
    | _ =>  pure $ Fail failures
  stdFinalFailure <- do
    Just (_, Just newF) <- pure stdFailure
      | e => pure $ fst <$> e
    newF
  fileFinalFailure <- do
    Just (_, Just newF) <- pure fileFailure
      | e => pure $ fst <$> e
    newF
  let (x :: xs) = catMaybes [statusFailure, stdFinalFailure, fileFinalFailure]
    | _ => pure Success
  pure $ Fail (x :: xs)

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
collectOutputs : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console
      ] e => App e (Either TestResult TestOutput)
collectOutputs = do
  t <- get CurrentTest
  debug $ withOffset 2 "Check pending"
  let False = t.pending
    | True => pure $ Left Skipped
  outputFile <- getOutputFile
  exitStatus <- runCommand outputFile
  output <- catchNew (readFile outputFile)
    (\e : FSError => throw $
          FileSystemError "Can't read output file \{outputFile}")
  let Just f = t.file
    | Nothing => pure $ Right $  MkTestOutput exitStatus output Nothing
  Just fileContent <- catchNew (Just <$> readFile f) (\err : FSError => pure Nothing)
    | Nothing => pure $ Left $ Fail [ExpectedFileNotFound f]
  pure $ Right $ MkTestOutput exitStatus output (Just fileContent)
  where
    runCommand : String -> App e Int
    runCommand outputFile = do
      t <- get CurrentTest
      inputFile <- generateInput
      let cmd = "(\{t.command}) \{maybe "" ("< " ++ )inputFile} > \"\{outputFile}\""
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
  Right outputs <- collectOutputs
    | Left res => pure res
  checkExpectations outputs

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
    continueIfExists _ = throw $ FileSystemError "Cant't create or accesse test directory"
    createTestDir : App e ()
    createTestDir = do
      testDir <- getSingleTestDir
      catchNew (createDir testDir) continueIfExists
    createGoldenDir : App e ()
    createGoldenDir = do
      goldenDir <- getSingleTestGoldenDir
      catchNew (createDir goldenDir) continueIfExists


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
  putStrLn $ withOffset 2 $ !red "\{!ko} \{t.name}: \{unwords $ map displayFailReason xs}"
  getContentMismatch xs
  where
    partialExpectation : String -> String
    partialExpectation x = case lines x of
      (head ::: tail) => unlines $ withOffset 6 ("- " ++ head) :: (withOffset 8 <$> tail)
    getContentMismatch : List FailReason -> App e ()
    getContentMismatch [] = pure ()
    getContentMismatch (WrongOutput src (DifferentOutput x y) :: _) =
      let d = case !(diff <$> get GlobalConfig) of
                   None => Native
                   d' => d'
      in showDiff d 6 x y
    getContentMismatch (WrongOutput src (PartialOutputMismatch order  xs ys) :: _) = do
      putStrLn $ withOffset 6 $ "Unmatched expectations:"
      traverse_ (putStrLn . partialExpectation) xs
    getContentMismatch (_ :: xs) = getContentMismatch xs

runAllTests : SystemIO (SystemError :: TestError :: e) =>
  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: TestError :: e) =>
  Console (TestError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Console
      ] e =>  TestPlan -> App e (List (Test, Either TestError TestResult))
runAllTests plan = do
  putStrLn $ separator 80
  putStrLn $ !bold "Running tests..."
  batchTests [] plan
  where
    processTest : Test -> App e (Test, Either TestError TestResult)
    processTest x = do

      rdir <- getReplicaDir
      let False = x.pending
        | True => pure (x, Right Skipped)
      r <- handle
             (new x runTest)
             (pure . MkPair x . Right)
             (\err : TestError => pure (x, Left err))
      pure r
    prepareBatch : Nat -> TestPlan -> (List Test, List Test)
    prepareBatch n plan = if n == 0
                             then (plan.now, Prelude.Nil)
                             else splitAt n plan.now
    processResult : TestPlan -> (Test, Either TestError TestResult) -> TestPlan
    processResult plan (t, Right Success) = validate t.name plan
    processResult plan (t, _) = fail t.name plan
    batchTests : List (Test, Either TestError TestResult) ->
                 TestPlan -> App e (List (Test, Either TestError TestResult))
    batchTests acc plan = do
      n <- threads <$> get RunContext
      case prepareBatch n plan of
           ([], later) => pure $ join
              [ acc
              , map (\t => (t, Left Inaccessible)) plan.later
              , map (\(reason, t) => (t, Left $ RequirementsFailed reason)) plan.skipped
              ]
           (now, nextBatches) => do
             res <- map await <$> traverse (map (fork . delay) . processTest) now
             when (not !(interactive <$> get RunContext))
               (traverse_ (\(t, r) => new t (testOutput r)) res)
             p <- punitive <$> get RunContext
             if p && any (not . isFullSuccess . snd) res
                then pure res
                else do
                   let plan' = record {now = nextBatches} plan
                   debug $ displayPlan plan'
                   batchTests (acc ++ res) $ assert_smaller plan (foldl processResult plan' res)

report : Console e => State GlobalConfig Global e => Stats -> App e ()
report x = do
  putStrLn $ separator 80
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
  pure $ foldl (\p, t => validate t.name p) (buildPlan selected) (r ++ rejected)

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
  catchNew (writeFile logFile (show $ reportToJSON $ map (mapFst name)  result))
    (\err : FSError => throw $ CantAccessTestFile logFile)
  when !(interactive <$> get RunContext)
    (do putStrLn $ separator 80
        putStrLn $ !bold "Test results:"
        traverse_ (uncurry (\t, r => new t (testOutput r))) result)
  let stats = asStats $ snd <$> result
  report $ stats
  pure stats
