module Main

import Replica

import System

usage : String
usage = "Usage: replica <test path>"

main : IO ()
main = do
  (_ :: path :: _) <- getArgs
    | other => putStrLn usage
  result <- runTest path
  displayTestResult path result
