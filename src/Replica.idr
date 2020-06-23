module Replica

import Data.List
import Data.Strings

import System
import System.Directory
import System.File
import System.Info

import Replica.TestConfig
import Replica.RunEnv

%default total


public export
data TestResult
  = Success
  | Failure (Maybe String) String
  | NewGolden String


public export
data TestError
  = CantLocateDir String
  | CantReadOutput FileError
  | CantParseTest (ParsingError (List BuildError))
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

covering export
displayResult : Either TestError TestResult -> IO ()
displayResult (Left (CantLocateDir x)) = putStrLn $ "ERROR: Cannot find test directory"
displayResult (Left (CantReadOutput x)) = putStrLn $ "ERROR: Cannot read test output"
displayResult (Left (CantReadExpected x)) = putStrLn $ "ERROR: Cannot read expected output"
displayResult (Left (CantParseTest x)) = putStrLn $ "ERROR: Parsing failed: " ++ displayParsingError (const "something is missing") x
displayResult (Left CantWriteNewGolden) = putStrLn $ "ERROR: Cannot write file 'expected'"
displayResult (Left (CommandFailed x)) = putStrLn $ "ERROR: Cannot run command - Exit code: " ++ show x
displayResult (Right Success) = putStrLn "ok"
displayResult (Right (NewGolden str)) = putStrLn "new golden value" *> putStrLn str
displayResult (Right (Failure expected given)) = do
  putStrLn "FAILURE"
  expectedVsGiven expected given

displayPath : Path -> String
displayPath (MkDPair path snd) = foldr1 (\x, y => x <+> "." <+> y) path

covering export
displayTestResult : String -> Either TestError TestResult -> IO ()
displayTestResult testPath result = do
  putStr $ testPath ++ ": "
  displayResult result

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
handleFailure : Maybe String -> String -> IO (Either TestError TestResult)
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

covering
testExecution : (interactive : Bool) -> TestConfig -> IO (Either TestError TestResult)
testExecution interactive opts = do
  removeFile opts.outputFile
  0 <- system $ commandLine opts
    | n => pure $ Left $ CommandFailed 0
  Right out <- readFile opts.outputFile
    | Left err => pure $ Left $ CantReadOutput err
  Right exp <- readFile "expected"
    | Left err => if interactive
                  then handleFailure Nothing out
                  else pure $ Left $ CantReadExpected err
  let result = normalize exp == normalize out
  if result
    then pure $ Right Success
    else if interactive
           then handleFailure (Just exp) out
           else pure $ Right $ Failure (Just exp) out

asPath : DPair (List String) NonEmpty -> String
asPath (MkDPair path snd) = foldr1 (\x,y => x <+> "/" <+> y) path

covering export
runTest : RunEnv -> IO (Either TestError TestResult)
runTest env = do
  Just cdir <- currentDir
    | Nothing => pure $ Left $ CantLocateDir "Can't resolve currentDir"
  True <- changeDir env.path
    | False => pure $ Left $ CantLocateDir $ "Can't locate " ++ show env.path
  Right cfg <- parseTestConfig "test.repl"
    | Left err => pure $ Left $ CantParseTest err
  result <- testExecution env.interactive cfg
  changeDir cdir
  pure $ result
