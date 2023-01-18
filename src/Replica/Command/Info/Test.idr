module Replica.Command.Info.Test

import Data.String

import Replica.Help
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Option.Types
import Replica.Other.Decorated

%default total

public export
record TestInfoCommand' (f : Type -> Type) where
  constructor MkTestInfo
  showExpectation : f Bool
  filter : Filter' f
  global : Global' f

public export
TestInfoCommand : Type
TestInfoCommand = Done TestInfoCommand'

export
TyMap TestInfoCommand' where
  tyMap func x = MkTestInfo
    (func x.showExpectation)
    (tyMap func x.filter) (tyMap func x.global)

export
TyTraversable TestInfoCommand' where
  tyTraverse func x = [| MkTestInfo
      (func x.showExpectation)
      (tyTraverse func x.filter) (tyTraverse func x.global)
      |]

export
Show TestInfoCommand where
  show i = unwords [ "MkTestInfo"
                   , show i.showExpectation
                   , show i.filter
                   , show i.global]

showExpectationPart : Part (Builder TestInfoCommand') Bool
showExpectationPart = inj $ MkOption
      ( singleton
        $ MkMod (singleton "expectations") ['e'] (Left True)
          "show expectation for each test")
      False
      go
  where
    go : Bool -> Builder TestInfoCommand' -> Either String (Builder TestInfoCommand')
    go = ifSame showExpectation
                (\x => {showExpectation := Right x})
                (const $ const "Contradictory values for expectations")


optParseInfo : OptParse (Builder TestInfoCommand') TestInfoCommand
optParseInfo = [|MkTestInfo
  (liftAp showExpectationPart)
  (embed filter (\x => {filter := x}) optParseFilter)
  (embed global (\x => {global := x}) optParseGlobal)
  |]

defaultInfo : Default TestInfoCommand'
defaultInfo = MkTestInfo
  (defaultPart showExpectationPart)
  defaultFilter
  defaultGlobal

export
withGivenGlobal : Default TestInfoCommand' -> Default Global' -> Default TestInfoCommand'
withGivenGlobal x g = {global := g <+> defaultGlobal} x

export
helpTestInfo : Help
helpTestInfo =
  commandHelp {b = Builder TestInfoCommand'}
    ("replica":::["info"]) "test" "Display information about tests"
    optParseInfo
    (Just "JSON_TEST_FILE")

export
parseTestInfo : Default Global' ->  List String -> ParseResult TestInfoCommand
parseTestInfo g xs = do
  builder <- parse
    helpTestInfo
    (initBuilder $ defaultInfo `withGivenGlobal` g)
    optParseInfo
    xs
  maybe (InvalidMix "No test file given") Done $ build builder
