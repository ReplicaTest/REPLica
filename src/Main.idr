module Main

import Replica
import Replica.Options

import System

usage : String
usage = "Usage: replica <test path> <exec name> [OPTIONS]"

main : IO ()
main = do
  (_ :: path :: args) <- getArgs
    | other => putStrLn usage
  let Just opts = options args
    | Nothing => putStrLn "Invalid options"
  result <- runTest opts path
  displayTestResult path result
