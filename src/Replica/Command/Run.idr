module Replica.Command.Run

import Data.List
import Data.String

import Replica.Help
import Replica.Option.Types
import public Replica.Other.Decorated
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
RunAction = Done RunAction'

TyMap RunAction' where
  tyMap func x =
    MkRunAction
      (func x.workingDir)
      (func x.interactive)
      (func x.threads)
      (func x.only)
      (func x.exclude)
      (func x.onlyTags)
      (func x.excludeTags)
      (func x.hideSuccess)
      (func x.lastFailures)
      (func x.punitive)
      (func x.file)

TyTraversable RunAction' where
  tyTraverse func x = [|
    MkRunAction
      (func x.workingDir)
      (func x.interactive)
      (func x.threads)
      (func x.only)
      (func x.exclude)
      (func x.onlyTags)
      (func x.excludeTags)
      (func x.hideSuccess)
      (func x.lastFailures)
      (func x.punitive)
      (func x.file)
      |]

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


fileParamPart : Part (Builder RunAction') String
fileParamPart = inj $ MkParam "JSON_FILE" Just go
  where
    go : String -> Builder RunAction' -> Either String (Builder RunAction')
    go = one file
             (\x => record {file = Right x})
             (\x, y => "More than one test file were given: \{y}, \{x}")

interactivePart : Part (Builder RunAction') Bool
interactivePart = inj $ MkOption
      (singleton $ MkMod (singleton "interactive") ['i'] (Left True)
            "(re)generate golden number if different/missing")
      False
      go
  where
    go : Bool -> Builder RunAction' -> Either String (Builder RunAction')
    go = ifSame interactive
                (\x => record {interactive = Right x})
                (const $ const "Contradictory values for interactive")

workingDirPart : Part (Builder RunAction') String
workingDirPart = inj $ MkOption
      (singleton $ MkMod ("working-dir" ::: ["wdir"]) ['w']
          (Right $ MkValue "DIR" Just)
          "set where is the test working directory")
      "."
      go
  where
    go : String -> Builder RunAction' -> Either String (Builder RunAction')
    go = one workingDir
             (\x => record {workingDir = Right x})
             (\x, y => "More than one working directony were given: \{y}, \{x}")


threadsPart : Part (Builder RunAction') Nat
threadsPart = inj $ MkOption
      (singleton $ MkMod (singleton "threads") ['n']
          (Right $ MkValue "n" parsePositive)
          "max number of threads (default 1, 0 for no thread limit)")
      1
      go
  where
    go : Nat -> Builder RunAction' -> Either String (Builder RunAction')
    go = one threads
             (\x => record {threads = Right x})
             (\x, y => "More than one threads values were given: \{show y}, \{show x}")

onlyPart : Part (Builder RunAction') (List String)
onlyPart = inj $ MkOption
      (singleton $ MkMod (singleton "only") ['n']
          (Right $ MkValue "testX,testY" $ Just . go)
          "a comma separated list of the tests to run")
      [] compose
      where
        go : String -> List String
        go = forget . split (== ',')
        compose : List String -> Builder RunAction' -> Either String (Builder RunAction')
        compose xs x = case either (const []) (intersect xs) x.exclude of
          [] => Right $ record {only $= Right . (++ xs) . either (const []) id} x
          xs => Left "Some tests were both included and excluded: \{show xs}"


excludePart : Part (Builder RunAction') (List String)
excludePart = inj $ MkOption
      (singleton $ MkMod (singleton "exclude") ['N']
          (Right $ MkValue "testX,testY" $ Just . go)
          "a comma separated list of the tests to exclude")
      [] compose
      where
        go : String -> List String
        go = forget . split (== ',')
        compose : List String -> Builder RunAction' -> Either String (Builder RunAction')
        compose xs x = case either (const []) (intersect xs) x.only of
          [] => Right $ record {exclude $= Right . (++ xs) . either (const []) id} x
          xs => Left "Some tests were both included and excluded: \{show xs}"

onlyTagsPart : Part (Builder RunAction') (List String)
onlyTagsPart = inj $ MkOption
      (singleton $ MkMod ("tags" ::: ["only-tags"]) ['t']
          (Right $ MkValue "TAGS" $ Just . go)
          "a comma separated list of the tags to run")
      [] compose
      where
        go : String -> List String
        go = forget . split (== ',')
        compose : List String -> Builder RunAction' -> Either String (Builder RunAction')
        compose xs x = case either (const []) (intersect xs) x.excludeTags of
          [] => Right $ record {onlyTags $= Right . (++ xs) . either (const []) id} x
          xs => Left "Some tags were both included and excluded: \{show xs}"

excludeTagsPart : Part (Builder RunAction') (List String)
excludeTagsPart = inj $ MkOption
      (singleton $ MkMod (singleton "exclude-tags") ['T']
          (Right $ MkValue "TAGS" $ Just . go)
          "a comma separated list of the tags to exclude")
      []
      compose
      where
        go : String -> List String
        go = forget . split (== ',')
        compose : List String -> Builder RunAction' -> Either String (Builder RunAction')
        compose xs x = case either (const []) (intersect xs) x.onlyTags of
          [] => Right $ record {excludeTags $= Right . (++ xs) . either (const []) id} x
          xs => Left "Some tags were both included and excluded: \{show xs}"


punitivePart : Part (Builder RunAction') Bool
punitivePart = inj $ MkOption
      (singleton $ MkMod ("punitive" ::: ["fail-fast"]) ['p']
          (Left True)
          "fail fast mode: stops on the first test that fails")
      False
      go
      where
        go : Bool -> Builder RunAction' -> Either String (Builder RunAction')
        go = ifSame punitive
                    (\x => record {punitive = Right x})
                    (const $ const "Contradictory values for punitive mode")

hideSuccessPart : Part (Builder RunAction') Bool
hideSuccessPart = inj $ MkOption
      (singleton $ MkMod (toList1 ["hide-success", "fail-only"]) []
          (Left True)
          "hide successful tests in the report")
      False
      go
      where
        go : Bool -> Builder RunAction' -> Either String (Builder RunAction')
        go = ifSame hideSuccess
                    (\x => record {hideSuccess = Right x})
                    (const $ const "Contradictory values for hide success mode")

lastFailuresPart : Part (Builder RunAction') Bool
lastFailuresPart = inj $ MkOption
      (singleton $ MkMod (singleton "last-fails") ['l']
          (Left True)
          "if a previous run fails, rerun only the tests that failed")
      False
      go
      where
        go : Bool -> Builder RunAction' -> Either String (Builder RunAction')
        go = ifSame lastFailures
                    (\x => record {lastFailures = Right x})
                    (const $ const "Contradictory values for last failures mode")


optParseRun : OptParse (Builder RunAction') RunAction
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

defaultRun : Default RunAction'
defaultRun = MkRunAction
       (defaultPart workingDirPart)
       (defaultPart interactivePart)
       (defaultPart threadsPart)
       (defaultPart onlyPart)
       (defaultPart excludePart)
       (defaultPart onlyTagsPart)
       (defaultPart excludeTagsPart)
       (defaultPart hideSuccessPart)
       (defaultPart lastFailuresPart)
       (defaultPart punitivePart)
       (defaultPart fileParamPart)

export
parseRun : List String -> Either String RunAction
parseRun ("run"::xs) = do
    builder <- parse (initBuilder defaultRun) optParseRun xs
    maybe (Left "Some mandatory settings are missing") Right $ build builder
parseRun _ = Left "Not a run action"

export
helpRun : (global : List1 Help) -> Help
helpRun global = commandHelp "run" "Run tests from a Replica JSON file" global
  optParseRun
  (prj fileParamPart)
