module Replica.Command.Run

import Data.String

import Replica.Help
import Replica.Option.Types
import Replica.Option.Validate
import Replica.Other.Validation

%default total

public export
record RunAction' (f : Type -> Type) where
  constructor MkRunAction
  workingDir : f String
  interactive : f Bool
  threads : f Nat
  file : f String

public export
RunAction : Type
RunAction = RunAction' Prelude.id

export
Semigroup (RunAction' List) where
  (<+>) x y =
    MkRunAction
      (x.workingDir ++ y.workingDir)
      (x.interactive ++ y.interactive)
      (x.threads ++ y.threads)
      (x.file ++ y.file)

export
Monoid (RunAction' List) where
  neutral = MkRunAction empty empty empty empty

Show RunAction where
  show x = unwords
    [ "MkRunAction"
    , show $ x.workingDir
    , show $ x.interactive
    , show $ x.threads
    , show $ x.file ]


fileParam : Param String
fileParam = MkParam "filename" Just

fileOption : String -> RunAction' List
fileOption x =
  record {file = [x]} (neutral {ty = RunAction' List})


interactive : FlagOption Bool
interactive = MkFlag
  (singleton "interactive") ['i']
  [] []
  "(re)generate golden number if different/missing"
  False
  True

interactiveOption : Bool -> RunAction' List
interactiveOption x =
  record {interactive = [x]} (neutral {ty = RunAction' List})


workingDir : ParamOption String
workingDir = MkOption
  ("working-dir" ::: ["wdir"])
  ['w']
  "set where is the test working directory"
  "."
  (MkParam "dirName" Just)

workingDirOption : String -> RunAction' List
workingDirOption x =
  record {workingDir = [x]} (neutral {ty = RunAction' List})


threads : ParamOption Nat
threads = MkOption
  (singleton "threads")
  ['n']
  "max number of threads (default 1, 0 for no thread limit)"
  1
  (MkParam "n" parsePositive)

threadsOption : Nat -> RunAction' List
threadsOption x =
  record {threads = [x]} (neutral {ty = RunAction' List})



parseRunOptions : List String -> Validation (List String) (RunAction' List)
parseRunOptions xs =
  either
    (\x => Error ["Unknnown option \{x}"])
    Valid
    $ parse [ map {f = Part} interactiveOption $ inj interactive
            , map {f = Part} workingDirOption $ inj workingDir
            , map {f = Part} threadsOption $ inj threads
            , map {f = Part} fileOption $ inj fileParam
            ] xs

validateRunAction : RunAction' List -> Validation (List String) RunAction
validateRunAction r
  = [| MkRunAction
    (oneValidate (inj workingDir) r.workingDir)
    (oneValidate (inj interactive) r.interactive)
    (oneValidate (inj threads) r.threads)
    (oneValidate (inj fileParam) r.file)
    |]

export
parseRun : List String -> Validation (List String) RunAction
parseRun ("run" :: xs)
  = case parseRunOptions xs of
                Valid x => validateRunAction x
                Error e => Error e
parseRun _ = empty

export
helpRun : (global : List1 Help) -> Help
helpRun global = commandHelp "run" "Run tests from a Replica JSON file" global
  (  (helpPart $ inj interactive)
  ++ (helpPart $ inj workingDir)
  ++ (helpPart $ inj threads)
  )
  (Just fileParam)
