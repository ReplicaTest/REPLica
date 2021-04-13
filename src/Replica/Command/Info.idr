module Replica.Command.Info

import Data.String

import Replica.Help
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Option.Types
import Replica.Other.Decorated

%default total

public export
record InfoCommand' (f : Type -> Type) where
  constructor MkInfo
  showExpectation : f Bool
  filter : Filter' f
  global : Global' f

public export
InfoCommand : Type
InfoCommand = Done InfoCommand'

export
TyMap InfoCommand' where
  tyMap func x = MkInfo
    (func x.showExpectation)
    (tyMap func x.filter) (tyMap func x.global)

export
TyTraversable InfoCommand' where
  tyTraverse func x = [| MkInfo
      (func x.showExpectation)
      (tyTraverse func x.filter) (tyTraverse func x.global)
      |]

export
Show InfoCommand where
  show i = unwords [ "MkInfo"
                   , show i.showExpectation
                   , show i.filter
                   , show i.global]

showExpectationPart : Part (Builder InfoCommand') Bool
showExpectationPart = inj $ MkOption
      ( singleton
        $ MkMod (singleton "expectations") ['e'] (Left True)
          "show expectation for each test")
      False
      go
  where
    go : Bool -> Builder InfoCommand' -> Either String (Builder InfoCommand')
    go = ifSame showExpectation
                (\x => record {showExpectation = Right x})
                (const $ const "Contradictory values for expectations")


optParseInfo : OptParse (Builder InfoCommand') InfoCommand
optParseInfo = [|MkInfo
  (liftAp showExpectationPart)
  (embed filter (\x => record {filter = x}) optParseFilter)
  (embed global (\x => record {global = x}) optParseGlobal)
  |]

defaultInfo : Default InfoCommand'
defaultInfo = MkInfo
  (defaultPart showExpectationPart)
  defaultFilter
  defaultGlobal

export
parseInfo : List1 String -> ParseResult InfoCommand
parseInfo ("info":::xs) = case parse (initBuilder defaultInfo) optParseInfo xs of
  InvalidMix reason => InvalidMix reason
  InvalidOption ys => InvalidMix "Unknown option: \{ys.head}"
  Done x => maybe (InvalidMix "File is not set") Done $ build x
parseInfo xs = InvalidOption xs

export
helpInfo : Help
helpInfo =
  commandHelp {b = Builder InfoCommand'}
    "info" "Display information about test suites"
    optParseInfo
    (Just "JSON_TEST_FILE")
