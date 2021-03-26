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
import Replica.Other.Decorated
import Replica.Other.Validation

%default total

covering
runRun : RunAction -> IO Int
runRun ctx = run $ new ctx.global $ new ctx $ handle runReplica
    (\stats => pure (cast $ stats.failures + stats.errors))
    (\err : ReplicaError => putStrLn (show err) >> pure 253)

covering
runInfo : InfoAction -> IO Int
runInfo info = run $ new info.global $ new info $ handle infoReplica
    (const $ pure 0)
    (\err : ReplicaError => putStrLn (show err) >> pure 253)

runHelp : Help -> IO ()
runHelp = putStrLn . display

covering
runCommand : Actions -> IO Int
runCommand a0 = let
  Left a1 = decomp a0
    | Right cmd => runRun cmd
  Left a2 = decomp a1
    | Right cmd => runInfo cmd
  in runHelp (decomp0 a2) $> 0

covering
main : IO ()
main = do
  (cmd::args) <- getArgs
    | _ => putStrLn "Error"
  let x = parseArgs args
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
         exitCode <- runCommand cmd
         exitWith $ case choose (exitCode == 0) of
           Right x => ExitFailure exitCode
           Left  x => ExitSuccess
