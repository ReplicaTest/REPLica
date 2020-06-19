module Replica

import Data.List
import Data.Strings

import System
import System.Directory
import System.File
import System.Info

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

export
displayResult : Either TestError TestResult -> IO ()
displayResult (Left x) = putStrLn $ "ERROR :Test cannot be performed"
displayResult (Right Success) = putStrLn "ok"
displayResult (Right (Failure expected given)) = do
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

testExecution : Path -> IO (Either TestError TestResult)
testExecution testPath = do
  system $ "sh ./run idris2 | tr -d '\\r' > output"
  Right out <- readFile "output"
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
runTest : Path -> IO (Either TestError TestResult)
runTest testPath  = do
  Just cdir <- currentDir
    | Nothing => pure $ Left $ CantLocateDir "Can't resolve currentDir"
  True <- changeDir testPath
    | False => pure $ Left $ CantLocateDir $ "Can't locate " ++ show testPath
  result <- testExecution testPath
  changeDir cdir
  pure $ result
