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

record Stats where
  constructor MkStats
  failures : Nat
  errors : Nat
  successes : Nat

buildStats : List (Either TestError TestResult) -> Stats
buildStats xs = let
  (fs, sts) = partitionEithers xs
  (neg, pos) = partition isFailure $ map status sts
  in MkStats (length fs) (length neg) (length pos)

displayStats: Stats -> IO ()
displayStats s =
  putStrLn "✅: \{show s.successes} ❌: \{show s.errors} (invalid tests: \{show s.failures})"

covering
main : IO ()
main = do
  (_ :: args) <- getArgs
    | other => putStrLn usage
  let opts = options args
  putStrLn "Final report:"
  for_ (optionsToRunargs opts) \env => do
    result <- runDir env
    traverse_ (displayResult env.value) result
    putStrLn "---------------------------------------"
    displayStats $ buildStats result
    putStrLn ""
    case partitionEithers result of
         ((x::xs), _) => exitWith $ ExitFailure 255
         ([], xs) => case filter isFailure $ map status xs of
                          [] => exitSuccess {a = Unit}
                          _  => exitFailure

