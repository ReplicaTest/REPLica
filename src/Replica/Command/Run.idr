||| Parser for command line argument of a `replica run command`
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
  timing : f Bool
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
      (func x.timing)
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
      (func x.timing)
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
    , show x.timing
    , show x.threads
    , show x.hideSuccess
    , show x.punitive
    , show x.filter
    , show x.global
    ]

||| `Run` option that handle if we run in interactive mode
interactivePart : Part (Builder RunCommand') Bool
interactivePart = inj $ MkOption
      (singleton $ MkMod (singleton "interactive") ['i'] (Left True)
            "(re)generate golden number if different/missing")
      False
      go
  where
    go : Bool -> Builder RunCommand' -> Either String (Builder RunCommand')
    go = ifSame interactive
                (\x => {interactive := Right x})
                (const $ const "Contradictory values for interactive")

||| `Run` option that handle if we display execution time
timingPart : Part (Builder RunCommand') Bool
timingPart = inj $ MkOption
  (toList1
     [ MkMod ("timing" ::: ["duration"]) ['d'] (Left True)
            "display execution time of each tests"
     , MkMod ("no-timing" ::: ["no-duration"]) ['D'] (Left False)
            "hide execution time of each tests"
     ]
  )
  True
  go
  where
    go : Bool -> Builder RunCommand' -> Either String (Builder RunCommand')
    go = ifSame interactive
                (\x => {timing := Right x})
                (const $ const "Contradictory values for timing")

||| `Run` option that define the working directory for the tests
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
             (\x => {workingDir := Right x})
             (\x, y => "More than one working directony were given: \{y}, \{x}")


||| `Run` option for the parralelism level
threadsPart : Part (Builder RunCommand') Nat
threadsPart = inj $ MkOption
      (singleton $ MkMod (singleton "threads") ['x']
          (Right $ MkValue "N" parsePositive)
          "max number of threads (default 1; 0 for no thread limit)")
      1
      go
  where
    go : Nat -> Builder RunCommand' -> Either String (Builder RunCommand')
    go = one threads
             (\x => {threads := Right x})
             (\x, y => "More than one threads values were given: \{show y}, \{show x}")

||| `Run` option to decide if we stop execution on the first failure
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
                    (\x => {punitive := Right x})
                    (const $ const "Contradictory values for punitive mode")

||| `Run` option to decide if we hide successful tests in the report
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
                    (\x => {hideSuccess := Right x})
                    (const $ const "Contradictory values for hide success mode")

||| `RunCommand` option parser
optParseRun : OptParse (Builder RunCommand') RunCommand
optParseRun =
    [| MkRunCommand
       (liftAp workingDirPart)
       (liftAp interactivePart)
       (liftAp timingPart)
       (liftAp threadsPart)
       (liftAp hideSuccessPart)
       (liftAp punitivePart)
       (embed filter (\x => {filter := x}) optParseFilter)
       (embed global (\x => {global := x}) optParseGlobal)
    |]

||| Default option for the `RunCommand`
defaultRun : Default RunCommand'
defaultRun = MkRunCommand
       (defaultPart workingDirPart)
       (defaultPart interactivePart)
       (defaultPart timingPart)
       (defaultPart threadsPart)
       (defaultPart hideSuccessPart)
       (defaultPart punitivePart)
       defaultFilter
       defaultGlobal

||| Add Global config to a RunCommand config
withGivenGlobal : Default RunCommand' -> Default Global' -> Default RunCommand'
withGivenGlobal x g = {global := g <+> defaultGlobal} x


||| Parser for `replica run` command
parseRun' : Help -> Default Global' -> List String -> ParseResult RunCommand
parseRun' help g xs = do
  builder <- parse
    help
    (initBuilder $ defaultRun `withGivenGlobal` g)
    optParseRun
    xs
  maybe (InvalidMix "No test file given") Done $ build builder

||| Help for `replica run` command
export
helpRun : Help
helpRun = commandHelp {b = Builder RunCommand'}
  (pure "replica") "run" "Run tests from a Replica JSON file"
  optParseRun
  (Just "JSON_TEST_FILE(S)")

||| Help for `replica test` command
export
helpTest : Help
helpTest = commandHelp {b = Builder RunCommand'}
  (pure "replica") "test" "Alias for 'replica run'"
  optParseRun
  (Just "JSON_TEST_FILE(S)")

export
parseRun : Default Global' -> List1 String -> ParseResult RunCommand
parseRun g ("run":::xs) = parseRun' helpRun g xs
parseRun g ("test":::xs) = parseRun' helpTest g xs
parseRun _ xs = InvalidOption Nothing xs
