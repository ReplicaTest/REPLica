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
import Replica.Other.Decorated
import Replica.Other.String
import Replica.Other.Validation

%default total

data RunContext : Type where

record TestPart where
  constructor MkTestPart
  source : Maybe String
  expected : Maybe String
  given : String

record TestsResultParts where
  constructor ResultParts
  statusResult : Maybe FailReason
  expectationResult : Maybe FailReason
  fileResult : Maybe FailReason

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
  pure rDir

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

expectedVsGiven : State GlobalConfig Global e =>
  Nat -> String -> String -> App e (List String)
expectedVsGiven k expected given = pure $ map (withOffset k) $
  ( "Expected:" :: map !red (forget $ lines expected)) ++
  ( "Given:" :: map !green (forget $ lines given))

nativeShow : State GlobalConfig Global e =>
  Console e => String -> String -> App e ()
nativeShow expected given =
  putStrLn $ unlines !(expectedVsGiven 0 expected given)

-- Provide different ways to show the difference between expectations and givens
showDiff : SystemIO (SystemError :: e) =>
  State GlobalConfig Global e =>
  State CurrentTest Test e =>
  Console e => DiffCommand -> String -> String -> App e ()
showDiff Native expected given = nativeShow expected given
showDiff Diff x y = catchNew
  (system $ "git diff --minimal --word-diff=color --no-index -- \{!getExpectedOutput} \{!getOutputFile}")
  (\err : SystemError => nativeShow x y)
showDiff GitDiff x y = catchNew
  (system $ "git diff --minimal --word-diff=color --no-index -- \{!getExpectedOutput} \{!getOutputFile}")
  (\err : SystemError => nativeShow x y)
showDiff (Custom z) x y = catchNew
  (system $ "\{z} \{!getExpectedOutput} \{!getOutputFile}")
  (\err : SystemError => nativeShow x y)

-- on mismatch, check if we should replace the golden value
askForNewGolden : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , Exception TestError
      , Console
      ] e => TestPart -> App e (Maybe FailReason)
askForNewGolden part = do
  t <- get CurrentTest
  putStrLn $ "\{t.name}: Golden value mismatch for \{maybe "standard output" ("file " ++) part.source}"
  showExpectedAndGiven part.expected part.given
  putStrLn $ "Do you want to \{maybe "set" (const "replace") part.expected} the golden value? [N/y]"
  if !readAnswer
     then do
       f <- expectedFile
       handle (writeFile f  part.given)
         (const $ pure Nothing)
         (\err : FSError => throw $ FileSystemError "Cannot write golden value")
     else pure . Just $ maybe
       (WrongOutput part.source (GoldenIsMissing part.given))
       (WrongOutput part.source . flip DifferentOutput part.given)
       part.expected
  where

    expectedFile : App e String
    expectedFile = maybe getExpectedOutput (const getExpectedFile) part.source

    readAnswer : App e Bool
    readAnswer = do
      answer <- getLine
      pure $ toLower answer `elem` ["y", "yes"]

    showExpectedAndGiven : Maybe String -> String -> App e ()
    showExpectedAndGiven old given = do
      let Just str = old
        | Nothing => do
          putStrLn "Expected: Nothing Found"
          putStrLn "Given:"
          putStrLn given
      showDiff !(diff <$> get GlobalConfig) str given

allParts : TestsResultParts ->  List (Maybe FailReason)
allParts x = [x.statusResult, x.expectationResult, x.fileResult]

checkOutput :  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console ] e =>
  (status : Int) ->
  (outputPart : TestPart) ->
  (filePart : Maybe TestPart) ->
  App e TestResult
checkOutput status outputPart filePart
  = do
    log $ withOffset 2 "Checking result"
    ctx <- get RunContext
    t <- get CurrentTest
    let results
      = ResultParts
          (checkStatus t.mustSucceed)
          (checkExpectation outputPart)
          (maybe Nothing checkExpectation filePart)
    debug $ withOffset 4 "Check skip"
    let False = t.pending
      | True => pure Skipped
    debug $ withOffset 4 "Check success"
    let (x::xs) = catMaybes $ allParts results
      | [] => pure Success
    debug $ withOffset 4
          "Check interactive: \{show $ isNothing results.statusResult} \{show ctx.interactive}"
    let (Nothing, True) = (results.statusResult, ctx.interactive)
      | _ =>  pure $ Fail $ x :: xs
    debug $ withOffset 4 "Ask for new golden"
    debug $ withOffset 6 "Ask for new golden for std: \{show $ isJust results.expectationResult}"
    eRes <- maybe (pure Nothing)
                  (const $ askForNewGolden outputPart)
                  results.expectationResult
    debug $ withOffset 6 "Ask for new golden for file: \{show $ isJust results.fileResult}"
    fRes <- maybe (pure Nothing)
                  (\p => maybe (pure Nothing)
                               (const $ askForNewGolden p)
                               results.fileResult
                  )
                  filePart
    let [] = catMaybes [results.statusResult, eRes, fRes]
      | xs => pure $ Fail xs
    pure Success
    where
      checkStatus : Maybe Bool -> Maybe FailReason
      checkStatus m = do
        s <- m
        guard ((not s && status == 0) || (s && status /= 0)) $> WrongStatus s
      checkExpectation : TestPart -> Maybe FailReason
      checkExpectation part =
        let Just exp = part.expected
              | Nothing => Just (WrongOutput part.source (GoldenIsMissing part.given))
        in guard (exp /= part.given) $> WrongOutput part.source (DifferentOutput exp part.given)


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

testCore : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console
      ] e => App e TestResult
testCore = do
  t <- get CurrentTest
  outputFile <- getOutputFile
  inputFile <- generateInput
  let cmd = "(\{t.command}) \{maybe "" ("< " ++ )inputFile} > \"\{outputFile}\""
  log $ withOffset 2 "Running command: \{cmd}"
  exitStatus <- handle (system cmd)
    (const $ pure 0)
    (\(Err n) => pure n)
  output <- catchNew (readFile $ outputFile)
    (\e : FSError => throw $
          FileSystemError "Can't read output file \{outputFile}")
  let outputPart = MkTestPart Nothing
       (t.expectation <|> !(getExpected !getExpectedOutput))
       output
  let Just f = t.file
    | Nothing => checkOutput exitStatus outputPart Nothing
  Just fileContent <- catchNew (Just <$> readFile f) (\err : FSError => pure Nothing)
    | Nothing => pure $ Fail [ExpectedFileNotFound f]
  let filePart = MkTestPart (Just f) !(getExpected !getExpectedFile) fileContent
  checkOutput exitStatus outputPart (Just filePart)

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
  testDir <- getSingleTestDir
  catchNew (createDir testDir) continueIfExists
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


testOutput :
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Console
      ] e => String -> Either TestError TestResult -> App e ()
testOutput name (Left y) = do
  putStr (withOffset 2 $ (!yellow "\{!err} \{name}: "))
  putStrLn (displayTestError y)
testOutput name (Right Skipped) =
   putStrLn $ withOffset 2 "\{!pending} \{name}"
testOutput name (Right Success) = do
  if !(hideSuccess <$> get RunContext)
     then pure ()
     else putStrLn $ withOffset 2 "\{!ok} \{name}"
testOutput name (Right (Fail xs)) = do
  putStrLn $ withOffset 2 $ !red "\{!ko} \{name}: \{unwords $ map displayFailReason xs}"
  let Just (expected, given) = getContentMismatch xs
    | Nothing => pure ()
  putStrLn $ unlines !(expectedVsGiven 6 expected given)
  where
    getContentMismatch : List FailReason -> Maybe (String, String)
    getContentMismatch [] = Nothing
    getContentMismatch (WrongOutput src (DifferentOutput x y) :: _) = Just (x, y)
    getContentMismatch (_ :: xs) = getContentMismatch xs

runAllTests : SystemIO (SystemError :: TestError :: e) =>
  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: TestError :: e) =>
  Console (TestError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Console
      ] e =>  TestPlan -> App e (List (String, Either TestError TestResult))
runAllTests plan = do
  putStrLn $ separator 80
  putStrLn $ !bold "Running tests..."
  batchTests [] plan
  where
    processTest : Test -> App e (String, Either TestError TestResult)
    processTest x = do

      rdir <- getReplicaDir
      let False = x.pending
        | True => pure (x.name, Right Skipped)
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
  catchNew (writeFile logFile (show $ reportToJSON result))
    (\err : FSError => throw $ CantAccessTestFile logFile)
  when !(interactive <$> get RunContext)
    (do putStrLn $ separator 80
        putStrLn $ !bold "Test results:"
        traverse_ (uncurry testOutput) result)
  let stats = asStats $ snd <$> result
  report $ stats
  pure stats
