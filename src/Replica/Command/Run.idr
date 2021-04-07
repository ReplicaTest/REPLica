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
record RunCommand' (f : Type -> Type) where
  constructor MkRunCommand
  workingDir : f String
  interactive : f Bool
  threads : f Nat
  hideSuccess : f Bool
  punitive : f Bool
  filter : Filter' f
  global : Global' f

public export
RunCommand : Type
RunCommand = Done RunCommand'

TyMap RunCommand' where
  tyMap func x =
    MkRunCommand
      (func x.workingDir)
      (func x.interactive)
      (func x.threads)
      (func x.hideSuccess)
      (func x.punitive)
      (tyMap func x.filter)
      (tyMap func x.global)

TyTraversable RunCommand' where
  tyTraverse func x = [|
    MkRunCommand
      (func x.workingDir)
      (func x.interactive)
      (func x.threads)
      (func x.hideSuccess)
      (func x.punitive)
      (tyTraverse func x.filter)
      (tyTraverse func x.global)
      |]

export
Show RunCommand where
  show x = unwords
    [ "MkRunCommand"
    , show x.workingDir
    , show x.interactive
    , show x.threads
    , show x.hideSuccess
    , show x.punitive
    , show x.filter
    , show x.global
    ]

interactivePart : Part (Builder RunCommand') Bool
interactivePart = inj $ MkOption
      (singleton $ MkMod (singleton "interactive") ['i'] (Left True)
            "(re)generate golden number if different/missing")
      False
      go
  where
    go : Bool -> Builder RunCommand' -> Either String (Builder RunCommand')
    go = ifSame interactive
                (\x => record {interactive = Right x})
                (const $ const "Contradictory values for interactive")

workingDirPart : Part (Builder RunCommand') String
workingDirPart = inj $ MkOption
      (singleton $ MkMod ("working-dir" ::: ["wdir"]) ['w']
          (Right $ MkValue "DIR" Just)
          "set where is the test working directory")
      "."
      go
  where
    go : String -> Builder RunCommand' -> Either String (Builder RunCommand')
    go = one workingDir
             (\x => record {workingDir = Right x})
             (\x, y => "More than one working directony were given: \{y}, \{x}")


threadsPart : Part (Builder RunCommand') Nat
threadsPart = inj $ MkOption
      (singleton $ MkMod (singleton "threads") ['n']
          (Right $ MkValue "N" parsePositive)
          "max number of threads (default 1; 0 for no thread limit)")
      1
      go
  where
    go : Nat -> Builder RunCommand' -> Either String (Builder RunCommand')
    go = one threads
             (\x => record {threads = Right x})
             (\x, y => "More than one threads values were given: \{show y}, \{show x}")

punitivePart : Part (Builder RunCommand') Bool
punitivePart = inj $ MkOption
      (singleton $ MkMod ("punitive" ::: ["fail-fast"]) ['p']
          (Left True)
          "fail fast mode: stops on the first test that fails")
      False
      go
      where
        go : Bool -> Builder RunCommand' -> Either String (Builder RunCommand')
        go = ifSame punitive
                    (\x => record {punitive = Right x})
                    (const $ const "Contradictory values for punitive mode")

hideSuccessPart : Part (Builder RunCommand') Bool
hideSuccessPart = inj $ MkOption
      (singleton $ MkMod (toList1 ["hide-success", "fail-only"]) []
          (Left True)
          "hide successful tests in the report")
      False
      go
      where
        go : Bool -> Builder RunCommand' -> Either String (Builder RunCommand')
        go = ifSame hideSuccess
                    (\x => record {hideSuccess = Right x})
                    (const $ const "Contradictory values for hide success mode")

optParseRun : OptParse (Builder RunCommand') RunCommand
optParseRun =
    [| MkRunCommand
       (liftAp workingDirPart)
       (liftAp interactivePart)
       (liftAp threadsPart)
       (liftAp hideSuccessPart)
       (liftAp punitivePart)
       (embed filter (\x => record {filter = x}) optParseFilter)
       (embed global (\x => record {global = x}) optParseGlobal)
    |]

defaultRun : Default RunCommand'
defaultRun = MkRunCommand
       (defaultPart workingDirPart)
       (defaultPart interactivePart)
       (defaultPart threadsPart)
       (defaultPart hideSuccessPart)
       (defaultPart punitivePart)
       defaultFilter
       defaultGlobal

export
parseRun : List1 String -> ParseResult RunCommand
parseRun ("run":::xs) = do
    case parse (initBuilder defaultRun) optParseRun xs of
         InvalidMix reason => InvalidMix reason
         InvalidOption ys  => InvalidMix $ "Unknown option: " ++ ys.head
         Done builder      => maybe (InvalidMix "No test file given") Done $ build builder
parseRun xs = InvalidOption xs

export
helpRun : Help
helpRun = commandHelp {b = Builder RunCommand'}
  "run" "Run tests from a Replica JSON file"
  optParseRun
  (Just "JSON_TEST_FILE")
