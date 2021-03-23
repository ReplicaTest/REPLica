module Replica.Command.Run

import Data.List
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
  only : f (List String)
  exclude : f (List String)
  onlyTags : f (List String)
  excludeTags : f (List String)
  hideSuccess : f Bool
  lastFailures : f Bool
  punitive : f Bool
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
      (x.exclude ++ y.exclude)
      (x.onlyTags ++ y.onlyTags)
      (x.excludeTags ++ y.excludeTags)
      (x.hideSuccess ++ y.hideSuccess)
      (x.lastFailures ++ y.lastFailures)
      (x.punitive ++ y.punitive)
      (x.file ++ y.file)

export
Monoid (RunAction' List) where
  neutral = MkRunAction
    empty empty empty empty empty
    empty empty empty empty empty
    empty

neutralRun : RunAction' List
neutralRun = neutral

export
Show RunAction where
  show x = unwords
    [ "MkRunAction"
    , show $ x.workingDir
    , show $ x.interactive
    , show $ x.threads
    , show $ x.only
    , show $ x.onlyTags
    , show $ x.exclude
    , show $ x.excludeTags
    , show $ x.hideSuccess
    , show $ x.lastFailures
    , show $ x.punitive
    , show $ x.file ]


fileParamPart : Part (RunAction' List) String
fileParamPart = inj $ MkParam "JSON_FILE" Just
                \x => record {file $= (x::)}

interactivePart : Part (RunAction' List) Bool
interactivePart = inj $ MkOption
      (singleton $ MkMod (singleton "interactive") ['i'] (Left True)
            "(re)generate golden number if different/missing")
      False
      \x => record {interactive $= (x::)}

workingDirPart : Part (RunAction' List) String
workingDirPart = inj $ MkOption
      (singleton $ MkMod ("working-dir" ::: ["wdir"]) ['w']
          (Right $ MkValue "DIR" Just)
          "set where is the test working directory")
      "."
      \x => record {workingDir $= (x::)}


threadsPart : Part (RunAction' List) Nat
threadsPart = inj $ MkOption
      (singleton $ MkMod (singleton "threads") ['n']
          (Right $ MkValue "n" parsePositive)
          "max number of threads (default 1, 0 for no thread limit)")
      1
      \x => record {threads $= (x::)}

onlyPart : Part (RunAction' List) (List String)
onlyPart = inj $ MkOption
      (singleton $ MkMod (singleton "only") ['n']
          (Right $ MkValue "testX,testY" $ Just . go)
          "a comma separated list of the tests to run")
      []
      \xs => record {only = [xs]}
      where
        go : String -> List String
        go = forget . split (== ',')


excludePart : Part (RunAction' List) (List String)
excludePart = inj $ MkOption
      (singleton $ MkMod (singleton "exclude") ['N']
          (Right $ MkValue "testX,testY" $ Just . go)
          "a comma separated list of the tests to exclude")
      []
      \xs => record {exclude = [xs]}
      where
        go : String -> List String
        go = forget . split (== ',')

onlyTagsPart : Part (RunAction' List) (List String)
onlyTagsPart = inj $ MkOption
      (singleton $ MkMod ("tags" ::: ["only-tags"]) ['t']
          (Right $ MkValue "TAGS" $ Just . go)
          "a comma separated list of the tags to run")
      []
      \xs => record {onlyTags = [xs]}
      where
        go : String -> List String
        go = forget . split (== ',')

excludeTagsPart : Part (RunAction' List) (List String)
excludeTagsPart = inj $ MkOption
      (singleton $ MkMod (singleton "exclude-tags") ['T']
          (Right $ MkValue "TAGS" $ Just . go)
          "a comma separated list of the tags to exclude")
      []
      \xs => record {excludeTags = [xs]}
      where
        go : String -> List String
        go = forget . split (== ',')


punitivePart : Part (RunAction' List) Bool
punitivePart = inj $ MkOption
      (singleton $ MkMod ("punitive" ::: ["fail-fast"]) ['p']
          (Left True)
          "fail fast mode: stops on the first test that fails")
      False
      \x => record {punitive $= (x::)}

hideSuccessPart : Part (RunAction' List) Bool
hideSuccessPart = inj $ MkOption
      (singleton $ MkMod (toList1 ["hide-success", "fail-only"]) []
          (Left True)
          "hide successful tests in the report")
      False
      \x => record {hideSuccess $= (x::)}

lastFailuresPart : Part (RunAction' List) Bool
lastFailuresPart = inj $ MkOption
      (singleton $ MkMod (singleton "last-fails") ['l']
          (Left True)
          "if a previous run fails, rerun only the tests that failed")
      False
      \x => record {lastFailures $= (x::)}

optParseRun : OptParse (RunAction' List) RunAction
optParseRun =
    [| MkRunAction
       (liftAp workingDirPart)
       (liftAp interactivePart)
       (liftAp threadsPart)
       (liftAp onlyPart)
       (liftAp excludePart)
       (liftAp onlyTagsPart)
       (liftAp excludeTagsPart)
       (liftAp hideSuccessPart)
       (liftAp lastFailuresPart)
       (liftAp punitivePart)
       (liftAp fileParamPart)
    |]

parseRunOptions : List String -> Validation (List String) (RunAction' List)
parseRunOptions xs = either (\x => Error ["Unknnown option \{x}"]) Valid
    $ parse optParseRun xs

validateRunAction : RunAction' List -> Validation (List String) RunAction
validateRunAction r
  = [| MkRunAction
    (oneValidate workingDirPart r.workingDir)
    (oneValidate interactivePart r.interactive)
    (oneValidate threadsPart r.threads)
    (Valid $ join r.only)
    (Valid $ join r.exclude)
    (Valid $ join r.onlyTags)
    (Valid $ join r.excludeTags)
    (oneValidate hideSuccessPart r.hideSuccess)
    (oneValidate lastFailuresPart r.lastFailures)
    (oneValidate punitivePart r.punitive)
    (oneValidate fileParamPart r.file)
    |]

export
test : List String ->
       Either (List1 String, RunAction' List)
              (List String, RunAction' List)
test = parse' neutral optParseRun

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
  optParseRun
  (prj fileParamPart)
