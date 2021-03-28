module Replica.Command.Run

import Data.List
import Data.String

import Replica.Help
import Replica.Option.Types
import public Replica.Option.Filter
import public Replica.Option.Global
import Replica.Other.Decorated

%default total

public export
record RunAction' (f : Type -> Type) where
  constructor MkRunAction
  workingDir : f String
  interactive : f Bool
  threads : f Nat
  hideSuccess : f Bool
  punitive : f Bool
  filter : Filter' f
  global : Global' f

public export
RunAction : Type
RunAction = Done RunAction'

TyMap RunAction' where
  tyMap func x =
    MkRunAction
      (func x.workingDir)
      (func x.interactive)
      (func x.threads)
      (func x.hideSuccess)
      (func x.punitive)
      (tyMap func x.filter)
      (tyMap func x.global)

TyTraversable RunAction' where
  tyTraverse func x = [|
    MkRunAction
      (func x.workingDir)
      (func x.interactive)
      (func x.threads)
      (func x.hideSuccess)
      (func x.punitive)
      (tyTraverse func x.filter)
      (tyTraverse func x.global)
      |]

export
Show RunAction where
  show x = unwords
    [ "MkRunAction"
    , show x.workingDir
    , show x.interactive
    , show x.threads
    , show x.hideSuccess
    , show x.punitive
    , show x.filter
    , show x.global
    ]

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
          (Right $ MkValue "N" parsePositive)
          "max number of threads (default 1; 0 for no thread limit)")
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
       (liftAp hideSuccessPart)
       (liftAp punitivePart)
       (embed filter (\x => record {filter = x}) optParseFilter)
       (embed global (\x => record {global = x}) optParseGlobal)
    |]

defaultRun : Default RunAction'
defaultRun = MkRunAction
       (defaultPart workingDirPart)
       (defaultPart interactivePart)
       (defaultPart threadsPart)
       (defaultPart hideSuccessPart)
       (defaultPart punitivePart)
       defaultFilter
       defaultGlobal

export
parseRun : List1 String -> ParseResult RunAction
parseRun ("run":::xs) = do
    case parse (initBuilder defaultRun) optParseRun xs of
         InvalidMix reason => InvalidMix reason
         InvalidOption ys  => InvalidMix $ "Unknown option: " ++ ys.head
         Done builder      => maybe (InvalidMix "No test file given") Done $ build builder
parseRun xs = InvalidOption xs

export
helpRun : Help
helpRun = commandHelp {b = Builder RunAction'}
  "run" "Run tests from a Replica JSON file"
  optParseRun
  (Just "JSON_TEST_FILE")
