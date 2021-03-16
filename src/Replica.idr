module Replica

import Data.So
import Data.String
import Data.List.AtIndex
import Data.OpenUnion


import System

import Replica.App
import Replica.Core
import Replica.Command
import Replica.Other.Validation

%default total

covering
runRun : RunAction -> IO Int
runRun ctx = run $ new ctx $ handle runReplica
    (\stats => pure (the Int $ cast $ stats.failures + stats.errors))
    (\err : ReplicaError => putStrLn (show err) >> pure 253)

covering
runInfo : InfoAction -> IO Int
runInfo info = run $ new info $ handle infoReplica
    (const $ pure 0)
    (\err : ReplicaError => putStrLn (show err) >> pure 253)

covering
runCommand : Actions -> IO Int
runCommand a0 = do
  let Left a1 = decomp a0
    | Right cmd => runRun cmd
  runInfo $ decomp0 a1


covering
main : IO ()
main = do
  (cmd::args) <- getArgs
    | _ => putStrLn "Error"
  let x = parseArgs args
  case x of
       Error [] => do
         putStrLn "usage: replica run [options]"
         exitWith $ ExitFailure 254
       Error es => do
         putStrLn "Can't parse command arguments:"
         putStrLn $ unlines es
         putStrLn "usage: replica run [options]"
       Valid cmd => do
         exitCode <- runCommand cmd
         exitWith $ case choose (exitCode == 0) of
           (Left x) => ExitSuccess
           (Right x) => ExitFailure exitCode
