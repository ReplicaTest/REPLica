module Replica

import Data.So
import Data.String
import Data.List.AtIndex
import Data.List
import Data.List1
import Data.OpenUnion


import System

import Replica.App
import Replica.Core
import Replica.Command
import Replica.Option.Parse
import Replica.Option.Types
import Replica.Other.Decorated
import Replica.Other.Validation

%default total

covering
runRun : RunCommand -> IO Int
runRun ctx = run $ new ctx.global $ new ctx $ handle runReplica
    (\stats => pure (cast $ stats.failures + stats.errors))
    (\err : ReplicaError => putStrLn (show err) >> pure 253)

covering
runInfo : InfoCommand -> IO Int
runInfo info = run $ new info.global $ new info $ handle infoReplica
    (const $ pure 0)
    (\err : ReplicaError => putStrLn (show err) >> pure 253)

runHelp : Help -> IO ()
runHelp = putStrLn . display

covering
runCommand : Commands -> IO Int
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
  let Just args' = toList1' args
    | Nothing => runHelp help
  gc <- givenConfig
  let x = parseArgs gc args'
  case x of
       InvalidMix e => do
         putStrLn e
         runHelp help
         exitWith $ ExitFailure 254
       InvalidOption ys => do
         putStrLn "Invalid command : \{ys.head}"
         runHelp help
         exitWith $ ExitFailure 254
       Done cmd => do
         exitCode <- runCommand cmd
         exitWith $ case choose (exitCode == 0) of
           Right x => ExitFailure exitCode
           Left  x => ExitSuccess
