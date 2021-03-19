module Replica.Command.Run

import Data.String

import Replica.Help
import Replica.Option.Types
import Replica.Option.Validate
import Replica.Other.Validation

%default total

namespace Types

  public export
  record RunAction' (f : Type -> Type) where
    constructor MkRunAction
    workingDir : f String
    interactive : f Bool
    threads : f Nat
    only : f (List String)
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
      (x.only ++ y.only)
      (x.file ++ y.file)

export
Monoid (RunAction' List) where
  neutral = MkRunAction empty empty empty empty empty

export
Show RunAction where
  show x = unwords
    [ "MkRunAction"
    , show $ x.workingDir
    , show $ x.interactive
    , show $ x.threads
    , show $ x.only
    , show $ x.file ]


fileParam : Param String
fileParam = MkParam "filename" Just

fileOption : String -> RunAction' List
fileOption x =
  record {file = [x]} (neutral {ty = RunAction' List})


interactivePart : Part Bool
interactivePart = inj interactive
  where
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

workingDirPart : Part String
workingDirPart = inj workingDir
  where
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


threadsPart : Part Nat
threadsPart = inj threads
  where
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

onlyPart : Part (List String)
onlyPart = inj only
  where
    only : ParamOption (List String)
    only = MkOption
      (singleton "only")
      []
      "a comma separated list of the tests to run"
      []
      (MkParam "testX,testY" $ Just . go)
      where
        go : String -> List String
        go = forget . split (== ',')


onlyOption : List String -> RunAction' List
onlyOption xs =
  record {only = [xs]} (neutral {ty = RunAction' List})

parseRunOptions : List String -> Validation (List String) (RunAction' List)
parseRunOptions xs =
  either (\x => Error ["Unknnown option \{x}"]) Valid
    $ parse [ map {f = Part} interactiveOption interactivePart
            , map {f = Part} workingDirOption workingDirPart
            , map {f = Part} threadsOption threadsPart
            , map {f = Part} onlyOption onlyPart
            , map {f = Part} fileOption $ inj fileParam
            ] xs

validateRunAction : RunAction' List -> Validation (List String) RunAction
validateRunAction r
  = [| MkRunAction
    (oneValidate workingDirPart r.workingDir)
    (oneValidate interactivePart r.interactive)
    (oneValidate threadsPart r.threads)
    (Valid $ join r.only)
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
  (  (helpPart {a = Bool} interactivePart)
  ++ (helpPart {a = String} workingDirPart)
  ++ (helpPart {a = Nat} threadsPart)
  ++ (helpPart {a = List String} onlyPart)
  )
  (Just fileParam)
