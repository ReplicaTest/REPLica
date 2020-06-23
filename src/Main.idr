module Main

import Replica
import Replica.Options
import Replica.RunEnv

import System

%default total

usage : String
usage = "Usage: replica <test path> [<test path>*]"

optionsToRunargs : Options -> List (RunEnv String)
optionsToRunargs (MkOptions interactive tests) = MkRunEnv interactive <$> tests


covering
main : IO ()
main = do
  (_ :: args) <- getArgs
    | other => putStrLn usage
  let opts = options args
  for_ (optionsToRunargs opts) \env => do
    result <- runDir env
    traverse_ (displayTestResult env.value) result
