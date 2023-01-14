||| Entry point of the Replica application.
||| It's where we call the parser,
||| call the corresponding command
||| and handle the result.
module Replica

import Data.So
import Data.String
import Data.List.AtIndex
import Data.List
import Data.List1
import Data.OpenUnion


import System
import System.File

import Replica.App
import Replica.Core
import Replica.Command
import Replica.Option.Parse
import Replica.Option.Types
import Replica.Other.Decorated
import Replica.Other.Validation

%default total

data ReplicaExit : Type where
  HasReplicaError : ReplicaError -> ReplicaExit
  HasTestErrors : (n : Int) -> (notZ : So (not $ n == 0)) -> ReplicaExit
  HasArgParsingError : (res : ParseResult a) -> (0 isErr : ParsingFailure res) => ReplicaExit
  HasEnvInitialisationError : ReplicaExit
  Success : ReplicaExit

exitCode : ReplicaExit -> ExitCode
exitCode (HasReplicaError (CantAccessTestFile str)) = ExitFailure 255
exitCode (HasReplicaError (InvalidJSON strs)) = ExitFailure 254
exitCode (HasTestErrors n notZ) = if n > 127
  then ExitFailure 128
  else ExitFailure n
exitCode (HasArgParsingError (InvalidOption xs)) = ExitFailure 253
exitCode (HasArgParsingError (InvalidMix str)) = ExitFailure 252
exitCode HasEnvInitialisationError = ExitFailure 255
exitCode Success = ExitSuccess

runHelp : File -> Help -> IO ()

toStdErr : String -> IO ()
toStdErr = ignore . fPutStrLn stderr

displayExit : ReplicaExit -> IO ()
displayExit (HasReplicaError x) = ignore $ fPutStrLn stderr $ show x
displayExit (HasTestErrors n notZ) = pure ()
displayExit (HasArgParsingError (InvalidOption xs)) = do
  ignore $ fPutStrLn stderr "Invalid command : \{xs.head}"
  runHelp stderr help
displayExit (HasArgParsingError (InvalidMix str)) = do
  ignore $ fPutStrLn stderr str
  runHelp stderr help
displayExit HasEnvInitialisationError =
  ignore $ fPutStrLn stderr "Can't init env"
displayExit Success = pure ()

exitReplica : ReplicaExit -> IO ()
exitReplica x = displayExit x >> exitWith (exitCode x)

covering
runRun : RunCommand -> IO ReplicaExit
runRun ctx = run $ new ctx.global $ new ctx $ handle runReplica
    (\stats => do
     let nbErrs = cast $ stats.failures + stats.errors
     pure $ case choose (nbErrs == 0) of
          Left _ => Success
          Right notZ => HasTestErrors nbErrs notZ
    )
    (pure . HasReplicaError)

covering
runInfo : InfoCommand -> IO ReplicaExit
runInfo info = run $ new info $ handle infoReplica
    (const $ pure Success)
    (pure . HasReplicaError)

runHelp h = ignore . fPutStrLn h . display

covering
runSet : SetCommand -> IO ReplicaExit
runSet x = do
  home <- getEnv "HOME"
  let Just gb = noGlobal
    | Nothing => putStrLn "Can't init env" >> pure HasEnvInitialisationError
  run $ new x $ new gb $ new home $ handle setReplica
    (const $ pure Success)
    (pure . HasReplicaError)
  where
    noGlobal : Maybe Global
    noGlobal = build $ initBuilder ({files := Just []} defaultGlobal)

covering
runNew : NewCommand -> IO ReplicaExit
runNew ctx = run $ new ctx $ handle newReplica
  (const $ pure Success)
  (pure . HasReplicaError)

covering
runCommand : Commands -> IO ReplicaExit
runCommand a0 = let
  Left a1 = decomp a0
    | Right cmd => runRun cmd
  Left a2 = decomp a1
    | Right cmd => runInfo cmd
  Left a3 = decomp a2
    | Right cmd => runSet cmd
  Left a4 = decomp a3
    | Right cmd => runNew cmd
  Left a5 = decomp a4
    | Right h => runHelp stdout h $> Success
  MkVersion v = (decomp0 a5)
  in putStrLn v $> Success

covering
main : IO ()
main = do
  (cmd::args) <- getArgs
    | _ => putStrLn "Error"
  let Just args' = toList1' args
    | Nothing => runHelp stdout help
  gc <- givenConfig
  let x = parseArgs gc args'
  case x of
       InvalidMix _ => do
         exitReplica $ HasArgParsingError x
       InvalidOption _ => do
         exitReplica $ HasArgParsingError x
       Done cmd => do
         result <- runCommand cmd
         exitReplica result
