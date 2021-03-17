module Replica.Command.Run

import Replica.Option.Types
import Replica.Option.Validate
import Replica.Other.Validation

%default total

testFile : Param String
testFile = MkParam "filename" Just

interactive : FlagOption Bool
interactive = MkFlag
  ("interactive" ::: [])
  ['i']
  []
  []
  "(re)generate golden number if different/missing"
  False
  True

workingDir : ParamOption String
workingDir = MkOption
  ("working-dir" ::: ["wdir"])
  ['w']
  "set where the test working directory"
  ".replica/test"
  (MkParam "dirName" Just)

public export
record RunAction' (f : Type -> Type) where
  constructor MkRunAction
  workingDir : f String
  interactive : f Bool
  file : f String

public export
RunAction : Type
RunAction = RunAction' Prelude.id


export
Semigroup (RunAction' List) where
  (<+>) (MkRunAction workingDirX interactiveX fileX)
        (MkRunAction workingDirY interactiveY fileY)
    = MkRunAction
        (workingDirX ++ workingDirY)
        (interactiveX ++ interactiveY)
        (fileX ++ fileY)

export
Monoid (RunAction' List) where
  neutral = MkRunAction empty empty empty

interactiveOption : Bool -> RunAction' List
interactiveOption x =
  record {interactive = [x]} (neutral {ty = RunAction' List})

workingDirOption : String -> RunAction' List
workingDirOption x =
  record {workingDir = [x]} (neutral {ty = RunAction' List})

fileOption : String -> RunAction' List
fileOption x =
  record {file = [x]} (neutral {ty = RunAction' List})


parseRunOptions : List String -> Validation (List String) (RunAction' List)
parseRunOptions xs =
  either
    (\x => Error ["Unknnown option \{x}"])
    Valid
    $ parse [ inj $ map interactiveOption interactive
            , inj $ map workingDirOption workingDir
            , inj $ map fileOption testFile
            ] xs

validateRunAction : RunAction' List -> Validation (List String) RunAction
validateRunAction r
  = [| MkRunAction
    (oneWithDefault "workingDir" workingDir.defaultValue r.workingDir)
    (oneWithDefault "interactive" interactive.defaultValue r.interactive)
    (one "filename" r.file)
    |]

export
parseRun : List String -> Validation (List String) RunAction
parseRun ("run" :: xs)
  = case parseRunOptions xs of
                Valid x => validateRunAction x
                Error e => Error e
parseRun _ = empty

