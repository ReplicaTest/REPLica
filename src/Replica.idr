module Replica

import Data.List
import Data.Strings

import System
import System.Directory
import System.File
import System.Info

import Replica.TestConfig
import Replica.RunEnv

public export
data TestResult
  = Success
  | Failure String String


public export
data TestError
  = CantLocateDir String
  | CantReadOutput FileError
  | CantParseTest (ParsingError BuildError)
  | CantReadExpected FileError
  | CommandFailed Int

export
displayResult : Either TestError TestResult -> IO ()
displayResult (Left (CantLocateDir x)) = putStrLn $ "ERROR: Cannot find test directory"
displayResult (Left (CantReadOutput x)) = putStrLn $ "ERROR: Cannot read test output"
displayResult (Left (CantReadExpected x)) = putStrLn $ "ERROR: Cannot read expected output"
displayResult (Left (CantParseTest x)) = putStrLn $ "ERROR: Parsing failed: " ++ displayParsingError (const "something is missing") x
displayResult (Left (CommandFailed x)) = putStrLn $ "ERROR: Cannot run command - Exit code: " ++ show x
displayResult (Right Success) = putStrLn "ok"
displayResult (Right (Failure expected given)) = do
  putStrLn "FAILURE"
  putStrLn "Expected:"
  putStrLn expected
  putStrLn "Given:"
  putStrLn given

displayPath : Path -> String
displayPath (MkDPair path snd) = foldr1 (\x, y => x <+> "." <+> y) path

export
displayTestResult : String -> Either TestError TestResult -> IO ()
displayTestResult testPath result = do
  putStr $ testPath ++ ": "
  displayResult result

-- on Windows, we just ignore backslashes and slashes when comparing,
-- similarity up to that is good enough. Leave errors that depend
-- on the confusion of slashes and backslashes to unix machines.
normalize : String -> String
normalize str =
    if isWindows
      then pack $ filter (\ch => ch /= '/' && ch /= '\\') (unpack str)
      else str

commandLine : TestConfig -> String
commandLine (MkTestConfig exec path params inputFile outputFile)
 = exec ++ " " ++ params ++ (maybe "" (" < " ++) inputFile) ++ " > " ++ outputFile

testExecution : TestConfig -> IO (Either TestError TestResult)
testExecution opts = do
  removeFile opts.outputFile
  0 <- system $ commandLine opts
    | n => pure $ Left $ CommandFailed 0
  Right out <- readFile opts.outputFile
    | Left err => pure $ Left $ CantReadOutput err
  Right exp <- readFile "expected"
    | Left err => pure $ Left $ CantReadExpected err -- todo interactive generation
  let result = normalize exp == normalize out
  if result
    then do
      pure $ Right Success
    else
      pure $ Right $ Failure exp out

asPath : DPair (List String) NonEmpty -> String
asPath (MkDPair path snd) = foldr1 (\x,y => x <+> "/" <+> y) path

export
runTest : RunEnv -> IO (Either TestError TestResult)
runTest env = do
  Just cdir <- currentDir
    | Nothing => pure $ Left $ CantLocateDir "Can't resolve currentDir"
  True <- changeDir env.path
    | False => pure $ Left $ CantLocateDir $ "Can't locate " ++ show env.path
  Right opts <- parseTestConfig "test.repl"
    | Left err => pure $ Left $ CantParseTest err
  result <- testExecution opts
  changeDir cdir
  pure $ result
