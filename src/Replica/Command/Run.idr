module Replica.Command.Run

import Replica.Option.Types
import Replica.Option.Validate
import Replica.Other.Validation

%default total

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
  (<+>) x y =
    MkRunAction
      (x.workingDir ++ y.workingDir)
      (x.interactive ++ y.interactive)
      (x.file ++ y.file)

export
Monoid (RunAction' List) where
  neutral = MkRunAction empty empty empty


fileParam : Param String
fileParam = MkParam "filename" Just

fileOption : String -> RunAction' List
fileOption x =
  record {file = [x]} (neutral {ty = RunAction' List})


interactive : FlagOption Bool
interactive = MkFlag
  ("interactive" ::: [])
  ['i']
  []
  []
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
  "set where the test working directory"
  ".replica/test"
  (MkParam "dirName" Just)

workingDirOption : String -> RunAction' List
workingDirOption x =
  record {workingDir = [x]} (neutral {ty = RunAction' List})


parseRunOptions : List String -> Validation (List String) (RunAction' List)
parseRunOptions xs =
  either
    (\x => Error ["Unknnown option \{x}"])
    Valid
    $ parse [ map {f = Part} interactiveOption $ inj interactive
            , map {f = Part} workingDirOption $ inj workingDir
            , map {f = Part} fileOption $ inj fileParam
            ] xs

validateRunAction : RunAction' List -> Validation (List String) RunAction
validateRunAction r
  = [| MkRunAction
    (oneValidate (inj workingDir) r.workingDir)
    (oneValidate (inj interactive) r.interactive)
    (oneValidate (inj fileParam) r.file)
    |]

export
parseRun : List String -> Validation (List String) RunAction
parseRun ("run" :: xs)
  = case parseRunOptions xs of
                Valid x => validateRunAction x
                Error e => Error e
parseRun _ = empty

