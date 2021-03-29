module Replica.Command.Info

import Data.String

import Replica.Help
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Option.Types
import Replica.Other.Decorated

%default total

public export
record InfoAction' (f : Type -> Type) where
  constructor MkInfo
  showExpectation : f Bool
  filter : Filter' f
  global : Global' f

public export
InfoAction : Type
InfoAction = Done InfoAction'

export
TyMap InfoAction' where
  tyMap func x = MkInfo
    (func x.showExpectation)
    (tyMap func x.filter) (tyMap func x.global)

export
TyTraversable InfoAction' where
  tyTraverse func x = [| MkInfo
      (func x.showExpectation)
      (tyTraverse func x.filter) (tyTraverse func x.global)
      |]

export
Show InfoAction where
  show i = unwords [ "MkInfo"
                   , show i.showExpectation
                   , show i.filter
                   , show i.global]

showExpectationPart : Part (Builder InfoAction') Bool
showExpectationPart = inj $ MkOption
      ( singleton
        $ MkMod (singleton "expectations") ['e'] (Left True)
          "show expectation for each test")
      False
      go
  where
    go : Bool -> Builder InfoAction' -> Either String (Builder InfoAction')
    go = ifSame showExpectation
                (\x => record {showExpectation = Right x})
                (const $ const "Contradictory values for expectations")


optParseInfo : OptParse (Builder InfoAction') InfoAction
optParseInfo = [|MkInfo
  (liftAp showExpectationPart)
  (embed filter (\x => record {filter = x}) optParseFilter)
  (embed global (\x => record {global = x}) optParseGlobal)
  |]

defaultInfo : Default InfoAction'
defaultInfo = MkInfo
  (defaultPart showExpectationPart)
  defaultFilter
  defaultGlobal

export
parseInfo : List1 String -> ParseResult InfoAction
parseInfo ("info":::xs) = case parse (initBuilder defaultInfo) optParseInfo xs of
  InvalidMix reason => InvalidMix reason
  InvalidOption ys => InvalidMix "Unknown option: \{ys.head}"
  Done x => maybe (InvalidMix "File is not set") Done $ build x
parseInfo xs = InvalidOption xs

export
helpInfo : Help
helpInfo =
  commandHelp {b = Builder InfoAction'}
    "info" "Display information about test suites"
    optParseInfo
    (Just "JSON_TEST_FILE")
