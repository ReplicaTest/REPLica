||| Functions that run a single Replica test
module Replica.App.Run.RunOne

import Control.App
import Control.App.Console

import Data.List1
import Data.Maybe
import Data.String
import Data.String.Extra

import System.Path

import Replica.App.Clock
import Replica.App.FileSystem
import Replica.App.Format
import Replica.App.Log
import Replica.App.Replica
import Replica.App.Run.Display
import Replica.App.Run.Types
import Replica.App.System

import Replica.Command.Run
import Replica.Core.Types
import Replica.Option.Global
import Replica.Other.Decorated
import Replica.Other.String

%default total

normalize : String -> String
normalize
  = removeTrailingNL -- remove last new line
  . unlines  -- concatenate lines
  . map unwords -- put words together
  . filter (not . null) -- remove empty lines
  . map (assert_total words) -- decompose in words
  . lines -- by lines


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
      , SystemClock
      , Exception TestError
      , Console
      ] e => App e (Either TestResult (Clock Duration, Nat))
generateOutputs = do
  t <- get CurrentTest
  debug $ withOffset 2 "Check pending"
  let False = t.pending
    | True => pure $ Left Skipped
  statusFile <- getStatusFile
  (d, _) <- runCommand !getOutputFile !getErrorFile statusFile
  Right . MkPair d . fromMaybe 0 . parsePositive <$>
    catchNew (readFile statusFile) (\err : FSError => pure "0")
  where
    runCommand : (outputFile, errorFile, statusFile : String) -> App e (Clock Duration, Int)
    runCommand outputFile errorFile statusFile = durationOf $ do
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


getExpectationFile : Has
  [ State CurrentTest Test
  , State GlobalConfig Global ] e => Part -> App e String
getExpectationFile StdOut = getExpectedOutput
getExpectationFile StdErr = getExpectedError
getExpectationFile (FileName x) = getExpectedFile x


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
  catchNew (Just <$> readFile !(getExpectationFile part)) (\_ : FSError => pure Nothing)

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

checkStatus : Maybe (Either Bool Nat) -> Nat -> Maybe FailReason
checkStatus Nothing y = Nothing
checkStatus (Just (Left True)) y = guard (y /= 0) $> WrongStatus y (Left True)
checkStatus (Just (Left False)) y = guard (y == 0) $> WrongStatus y (Left False)
checkStatus (Just (Right x)) y = guard (y /= x) $> WrongStatus y (Right x)

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

checkContentWith :
   Alternative m =>
   (String -> String -> Bool) ->
   (spaceSensitive : Bool) ->
   (expected, given : String) ->
   m ()
checkContentWith f spaceSensitive expected given =
  guard $ f (normalizeContent expected) (normalizeContent given)
  where
    normalizeContent : String -> String
    normalizeContent c = if spaceSensitive then c else normalize c

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
  Maybe (exp : Expectation ** ExpectationError exp)
checkContent spaceSensitive given _ exp@(Exact expected) =
  checkContentWith (/=) spaceSensitive expected given $> (exp ** ())
checkContent spaceSensitive given _ exp@(StartsWith expected) =
  checkContentWith (\e, g => not $ e `isPrefixOf` g)
    spaceSensitive expected given $> (exp ** ())
checkContent spaceSensitive given _ exp@(EndsWith expected) =
  checkContentWith (\e, g => not $ e `isSuffixOf` g)
    spaceSensitive expected given $> (exp ** ())
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
checkContent spaceSensitive given (Just expected) Generated =
  checkContentWith (/=) spaceSensitive expected given $>
    (Generated ** Just expected)

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


checkExpectations :  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , Exception TestError
      , Console ] e =>
  (exitCode : Nat) ->
  (duration : Clock Duration) ->
  App e TestResult
checkExpectations exitCode duration = do
  log $ withOffset 2 "Checking expectations"
  t <- get CurrentTest
  ctx <- get RunContext
  let statusError = checkStatus t.status exitCode
  expResults <- traverse (uncurry checkOutput) t.expectations
  let failures = maybe id (::) statusError $ catMaybes expResults
  debug $ withOffset 4 "Check success"
  let (x :: xs) = failures
    | _ => pure $ Success duration -- exit on success
  let Nothing = statusError
    | _ => pure $ Fail failures -- quick exit on exit status error
  debug $ withOffset 4
        "Check interactive: \{show $ isNothing statusError} \{show ctx.interactive}"
  let True = ctx.interactive
    | _ =>  pure $ Fail failures -- exit on failure on non interactive mode
  [] <- map catMaybes $ traverse askForGolden failures
    | xs => pure $ Fail xs -- fail if we didn't accept all the new golden values
  pure $ Success duration
  where
    askForGolden : FailReason -> App e (Maybe FailReason)
    askForGolden (WrongOutput x given ((Generated ** expected) ::: [])) =
      interactiveGolden x given expected
    askForGolden x = pure $ Just x

testCore : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , SystemClock
      , Exception TestError
      , Console
      ] e => App e TestResult
testCore = do
  Right (duration, exitCode) <- generateOutputs
    | Left res => pure res
  checkExpectations exitCode duration

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
  logPhase phase
  handle (system "(\{x}) 1> /dev/null 2> /dev/null")
    (const $ runAll phase liftError xs)
    (\err : SystemError => throw $ liftError x)
  where
    logPhase : Maybe String -> App e ()
    logPhase = maybe (pure ()) (\p => log "\{p}: \{x}")

-- the whole test execution, including pre and post operation
performTest : SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State GlobalConfig Global
      , State RunContext RunCommand
      , SystemClock
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
runTest :
  SystemIO (SystemError :: e) =>
  FileSystem (FSError :: e) =>
  Has [ State CurrentTest Test
      , State RunContext RunCommand
      , State GlobalConfig Global
      , SystemClock
      , Exception TestError
      , Console
      ] e => App e TestResult
runTest = do
  ctx <- get RunContext
  t <- get CurrentTest
  createTestDir
  createGoldenDir
  let wd = fromMaybe ctx.workingDir t.workingDir
  logTestStart t wd
  catchNew (inDir wd performTest)
    (\err : FSError => throw $ FileSystemError
      "Error: cannot enter or exit test working directory \{show wd}")
  where

    logTestStart :
      Has [ Log ] e => Test -> String -> App e ()
    logTestStart t wd = do
      log "Executing \{t.name}"
      debug $ withOffset 2 $ show t
      log   $ withOffset 2 "Working directory: \{show wd}"

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

-- run a test if ready
export
processTest :
  SystemIO (SystemError :: TestError :: e) =>
  FileSystem (FSError :: TestError :: e) =>
  SystemClock (TestError :: e) =>
  Console (TestError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , Console
      ] e =>
  Test -> App e (Test, Either TestError TestResult)
processTest x = do
  let False = x.pending
    | True => pure (x, Right Skipped)
  handle (new x runTest)
    (pure . MkPair x . Right)
    (\err : TestError => pure (x, Left err))
