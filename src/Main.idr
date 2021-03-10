module Main

import Replica
import Replica.Options
import Replica.RunEnv

import Data.List
import Data.Either

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
    traverse_ (displayResult env.value) result
    case partitionEithers result of
         ((x::xs), _) => exitWith $ ExitFailure 255
         ([], xs) => case filter isFailure $ map status xs of
                          [] => exitSuccess {a = Unit}
                          _  => exitFailure

