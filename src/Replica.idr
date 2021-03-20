module Replica

import Data.So
import Data.String
import Data.List.AtIndex
import Data.List1
import Data.OpenUnion


import System

import Replica.App
import Replica.Core
import Replica.Command
import Replica.App
import Replica.Option.Global
import Replica.Other.Validation

%default total

covering
runRun : GlobalOption -> RunAction -> IO Int
runRun opts ctx = run $ new opts $ new ctx $ handle runReplica
    (\stats => pure (the Int $ cast $ stats.failures + stats.errors))
    (\err : ReplicaError => putStrLn (show err) >> pure 253)

covering
runInfo : GlobalOption -> InfoAction -> IO Int
runInfo opts info = run $ new opts $ new info $ handle infoReplica
    (const $ pure 0)
    (\err : ReplicaError => putStrLn (show err) >> pure 253)

runHelp : Help -> IO ()
runHelp h = putStrLn (display h)

covering
runCommand : GlobalOption -> Actions -> IO Int
runCommand opts a0 = let
  Left a1 = decomp a0
    | Right cmd => runRun opts cmd
  Left a2 = decomp a1
    | Right cmd => runInfo opts cmd
  in runHelp (decomp0 a2) $> 0

covering
main : IO ()
main = do
  (cmd::args) <- getArgs
    | _ => putStrLn "Error"
  let Valid (cmdArgs, opts) = parseGlobal args
    | Error es => do
         putStrLn "Can't parse command arguments:"
         putStrLn $ unlines es
         putStrLn "usage: replica run [options]"
  let x = parseArgs cmdArgs
  case x of
       Error [] => do
         runHelp help
         exitWith $ ExitFailure 254
       Error es => do
         putStrLn "Can't parse command arguments:"
         putStrLn $ unlines es
         runHelp help
         exitWith $ ExitFailure 254
       Valid cmd => do
         exitCode <- runCommand opts cmd
         exitWith $ case choose (exitCode == 0) of
           (Left x) => ExitSuccess
           (Right x) => ExitFailure exitCode
