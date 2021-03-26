module Replica.Command.Run

import Data.List
import Data.String

import Replica.Help
import Replica.Option.Types
import public Replica.Option.Filter
import public Replica.Other.Decorated

%default total

public export
record RunAction' (f : Type -> Type) where
  constructor MkRunAction
  workingDir : f String
  interactive : f Bool
  threads : f Nat
  filter : Filter' f
  hideSuccess : f Bool
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
      (tyMap func x.filter)
      (func x.hideSuccess)
      (func x.punitive)
      (func x.file)

TyTraversable RunAction' where
  tyTraverse func x = [|
    MkRunAction
      (func x.workingDir)
      (func x.interactive)
      (func x.threads)
      (tyTraverse func x.filter)
      (func x.hideSuccess)
      (func x.punitive)
      (func x.file)
      |]

export
Show RunAction where
  show x = unwords
    [ "MkRunAction"
    , show x.workingDir
    , show x.interactive
    , show x.threads
    , show x.filter
    , show x.hideSuccess
    , show x.punitive
    , show x.file ]

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

optParseRun : OptParse (Builder RunAction') RunAction
optParseRun =
    [| MkRunAction
       (liftAp workingDirPart)
       (liftAp interactivePart)
       (liftAp threadsPart)
       (embed filter (\x => record {filter = x}) optParseFilter)
       (liftAp hideSuccessPart)
       (liftAp punitivePart)
       (liftAp fileParamPart)
    |]

defaultRun : Default RunAction'
defaultRun = MkRunAction
       (defaultPart workingDirPart)
       (defaultPart interactivePart)
       (defaultPart threadsPart)
       defaultFilter
       (defaultPart hideSuccessPart)
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
