module Replica

import Data.List
import Data.Strings

import System
import System.Directory
import System.File
import System.Info

import Replica.RunEnv
import Replica.Suite
import Replica.Text.Lexer
import Replica.Text.Parser
import Replica.TestConfig.Parser
import Replica.Suite.Parser
import Replica.TestConfig
import Replica.Validation

%default total


public export
data TestStatus
  = Success
  | Failure (Maybe String) String
  | NewGolden String

public export
record TestResult where
  constructor MkTestResult

  test : TestConfig
  status : TestStatus

public export
data TestError
  = CantLocateDir String
  | CantReadOutput FileError
  | CantParseTest (ParsingError (List TestConfig.Core.BuildError))
  | CantParseSuite (ParsingError (List Suite.Core.BuildError))
  | CantParse (ParsingError Void)
  | CantReadExpected FileError
  | CantWriteNewGolden
  | CommandFailed Int

expectedVsGiven : Maybe String -> String -> IO ()
expectedVsGiven exp out = do
  case exp of
       Nothing => putStrLn "Expected: Nothing Found"
       Just str => do
         putStrLn "Expected:"
         putStrLn str
  putStrLn "Given:"
  putStrLn out

covering
displayStatus : TestStatus -> IO ()
displayStatus Success = putStrLn "ok"
displayStatus (NewGolden str) = putStrLn "new golden value" *> putStrLn str
displayStatus (Failure expected given) = do
  putStrLn "FAILURE"
  expectedVsGiven expected given

displayPath : Path -> String
displayPath (MkDPair path snd) = foldr1 (\x, y => x <+> "." <+> y) path

covering export
displayResult : String -> Either TestError TestResult -> IO ()
displayResult str (Left err) = do
  putStr $ str ++ ": "
  putStrLn $ case err of
    CantLocateDir x => "ERROR: Cannot find test directory"
    CantReadOutput x => "ERROR: Cannot read test output"
    CantReadExpected x => "ERROR: Cannot read expected output"
    CantParseTest x => "ERROR: Parsing failed: " ++ displayParsingError (const "something is missing") x
    CantParseSuite x => "ERROR: Parsing failed: " ++ displayParsingError (const "something is missing") x
    CantParse x => "ERROR: Parsing failed: " ++ displayParsingError (const "something is missing") x
    CantWriteNewGolden => "ERROR: Cannot write file 'expected'"
    CommandFailed x => "ERROR: Cannot run command - Exit code: " ++ show x
displayResult _ (Right ts) = do
  putStr $ displayPath ts.test.path ++ ": "
  displayStatus ts.status

-- on Windows, we just ignore backslashes and slashes when comparing,
-- similarity up to that is good enough. Leave errors that depend
-- on the confusion of slashes and backslashes to unix machines.
covering
normalize : String -> String
normalize str =
    if isWindows
      then pack $ filter (\ch => ch /= '/' && ch /= '\\') (unpack str)
      else str

commandLine : TestConfig -> String
commandLine (MkTestConfig exec path params inputFile outputFile)
 = exec ++ " " ++ params ++ (maybe "" (" < " ++) inputFile) ++ " > " ++ outputFile

covering
handleFailure : Maybe String -> String -> IO (Either TestError TestStatus)
handleFailure exp out = do
  expectedVsGiven exp out
  putStrLn $ "Do you want to " ++ maybe "set" (const "replace") exp ++ " the golden value? [N/y]"
  if !readAnswer
     then do
       Right _ <- writeFile "expected" out
         | Left err => pure $ Left $ CantWriteNewGolden
       putStrLn "New golden value saved"
       pure $ Right $ NewGolden out
     else do
       putStrLn "Resuming..."
       pure $ Right $ Failure exp out
  where
    covering
    readAnswer : IO Bool
    readAnswer = do
      answer <- getLine
      case answer of
           ""  => pure False
           "n" => pure False
           "N" => pure False
           "y" => pure True
           "Y" => pure True
           _ => putStrLn "I didn't understand your answer. [N/y]" *> readAnswer

covering export
inDir : RunEnv String -> IO (List (Either TestError a)) -> IO (List (Either TestError a))
inDir env action = do
  Just origin <- currentDir
    | Nothing => pure [Left $ CantLocateDir "Can't resolve currentDir"]
  True <- changeDir env.value
    | False => pure [Left $ CantLocateDir $ "Can't locate " ++ show env.value]
  res <- action
  True <- changeDir origin
    | False => pure [Left $ CantLocateDir $ "Can't goback to " ++ show origin]
  pure res

covering
runTest : RunEnv TestConfig -> IO (Either TestError TestResult)
runTest env = do
  removeFile env.value.outputFile
  0 <- system $ commandLine env.value
    | n => pure $ Left $ CommandFailed 0
  Right out <- readFile env.value.outputFile
    | Left err => pure $ Left $ CantReadOutput err
  Right exp <- readFile "expected"
    | Left err => if env.interactive
                  then map (map $ MkTestResult env.value)  $ handleFailure Nothing out
                  else pure $ Left $ CantReadExpected err
  let result = normalize exp == normalize out
  if result
    then pure $ Right $ MkTestResult env.value Success
    else if env.interactive
           then map (map $ MkTestResult env.value) $ handleFailure (Just exp) out
           else pure $ Right $ MkTestResult env.value $ Failure (Just exp) out

asPath : DPair (List String) NonEmpty -> String
asPath (MkDPair path snd) = foldr1 (\x,y => x <+> "/" <+> y) path


covering
parseTestFile : IO (Either TestError (Either TestConfig Suite))
parseTestFile = do
  Right content <- readFile "test.repl"
    | Left err => pure $ Left $ CantParse $ FileNotFound err
  pure $ processContent content
  where

    processContent : String -> Either TestError (Either TestConfig Suite)
    processContent content = do
      tokens <- mapError (CantParse . LexerFailed) $ lex content
      (parseRes, []) <- mapError (CantParse . ParserFailed) $ parse
          (   map (mapError (CantParseTest . TargetError) . toEither . map Left) testConfig
          <|> map (mapError (CantParseSuite . TargetError) . toEither . map Right) suite
          ) tokens
        | (_, err) => Left $ CantParse $ ParserFailed $ Error "Cannot parse tokens"  err
      parseRes

covering export
runDir : RunEnv String -> IO (List (Either TestError TestResult))
runDir x = inDir x $ do
  (Right testOrSuite) <- parseTestFile
    | Left err => pure [Left err]
  either
    (\t => map pure . runTest . setValue t)
    (\s => runSuite . setValue s)
    testOrSuite x

  where

    covering
    runSuite : RunEnv Suite -> IO (List (Either TestError TestResult))
    runSuite env = map concat $ for env.value.tests \filename =>
      runDir $ setValue (asPath filename) env
