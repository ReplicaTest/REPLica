module Replica

import Data.So
import Data.String

import System

import Replica.App
import Replica.Core
import Replica.Command
import Replica.Other.Validation

%default total

covering
main : IO ()
main = do
  (cmd::args) <- getArgs
    | _ => putStrLn "Error"
  let Just (Valid ctx) = parseAction args
    | Just (Error err) => do
        putStrLn $ unlines err
        exitWith {a = ()} $ ExitFailure 255
    | Nothing => case args of
                      [] => do
                         putStrLn "usage: replica run [options]"
                         exitWith $ ExitFailure 254
                      (x :: xs) => do
                         putStrLn "Unknown action \{show x}"
                         exitWith $ ExitFailure 254
  exitCode <- run $ new ctx $ handle runReplica
                       (\stats => pure (the Int $ cast $ stats.failures + stats.errors))
                       (\err : ReplicaError => putStrLn (show err) >> pure 253)
  exitWith $ case choose (exitCode == 0) of
     (Left x) => ExitSuccess
     (Right x) => ExitFailure exitCode
