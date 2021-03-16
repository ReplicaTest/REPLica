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
  not

workingDir : ParamOption String
workingDir = MkOption
  ("working-dir" ::: ["wdir"])
  ['w']
  "set where the test are run"
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

interactiveOption : Bool -> RunAction' List -> RunAction' List
interactiveOption x = record {interactive $= (x::)}

workingDirOption : String -> RunAction' List -> RunAction' List
workingDirOption x = record {workingDir $= (x::)}

fileOption : String -> RunAction' List -> RunAction' List
fileOption x = record {file $= (x::)}


parseRunOptions : List String -> RunAction' List ->
  Validation (List String) (RunAction' List)
parseRunOptions [] a = Valid a
parseRunOptions xs@(x::tail) a = do
  let Just (f, xs')
      = map (mapFst interactiveOption) (parseFlagOption interactive xs)
        <|> map (mapFst workingDirOption) (parseParamOption workingDir xs)
        <|> (guard (tail == []) $> (fileOption x, tail))
    | Nothing => Error ["Unknnown option \{x}"]
  assert_total $ parseRunOptions xs' $ f a

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
  = case parseRunOptions xs neutral of
                Valid x => validateRunAction x
                Error e => Error e
parseRun _ = empty

