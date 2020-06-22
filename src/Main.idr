module Main

import Replica
import Replica.Options
import Replica.RunEnv

import System

%default total

usage : String
usage = "Usage: replica <test path> [<test path>*]"

optionsToRunargs : Options -> List RunEnv
optionsToRunargs (MkOptions interactive tests) = MkRunEnv interactive <$> tests


main : IO ()
main = do
  (_ :: args) <- getArgs
    | other => putStrLn usage
  let opts = options args
  for_ (optionsToRunargs opts) \env => do
    result <- runTest env
    displayTestResult env.path result
