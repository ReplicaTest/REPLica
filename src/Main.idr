module Main

import Replica
import Replica.Options

import System

usage : String
usage = "Usage: replica <test path> [<test path>*]"

main : IO ()
main = do
  (_ :: test1 :: args) <- getArgs
    | other => putStrLn usage
  for_ (test1 :: args) \path => do
    result <- runTest path
    displayTestResult path result
