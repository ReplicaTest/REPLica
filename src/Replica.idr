module Replica

import Data.List
import Data.Strings

import System
import System.Directory
import System.File
import System.Info

import Replica.Options

public export
Path : Type
Path = String

public export
data TestResult
  = Success
  | Failure String String


public export
data TestError
  = CantLocateDir String
  | CantReadOutput FileError
  | CantReadExpected FileError
  | CommandFailed Int

export
displayResult : Either TestError TestResult -> IO ()
displayResult (Left (CantLocateDir x)) = putStrLn $ "ERROR: Cannot find test directory"
displayResult (Left (CantReadOutput x)) = putStrLn $ "ERROR: Cannot read test output"
displayResult (Left (CantReadExpected x)) = putStrLn $ "ERROR: Cannot read expected output"
displayResult (Left (CommandFailed x)) = putStrLn $ "ERROR: Cannot run command - Exit code: " ++ show x
displayResult (Right Success) = putStrLn "ok"
displayResult (Right (Failure expected given)) = do
  putStrLn "FAILURE"
  putStrLn "Expected:"
  putStrLn expected
  putStrLn "Given:"
  putStrLn given

export
displayTestResult : Path -> Either TestError TestResult -> IO ()
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

testExecution : Options -> Path -> IO (Either TestError TestResult)
testExecution opts testPath = do
  removeFile opts.outputFile
  0 <- system $ opts.exec ++ " " ++ opts.params ++ (maybe "" (" < " ++) opts.input) ++ " > " ++ opts.outputFile
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

export
runTest : Options -> Path -> IO (Either TestError TestResult)
runTest opts testPath  = do
  Just cdir <- currentDir
    | Nothing => pure $ Left $ CantLocateDir "Can't resolve currentDir"
  True <- changeDir testPath
    | False => pure $ Left $ CantLocateDir $ "Can't locate " ++ show testPath
  result <- testExecution opts testPath
  changeDir cdir
  pure $ result
