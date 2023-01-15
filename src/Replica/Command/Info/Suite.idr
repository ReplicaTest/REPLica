module Replica.Command.Info.Suite

import Data.String

import Replica.Help
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Option.Types
import Replica.Other.Decorated

public export
record SuiteInfoCommand' (f : Type -> Type) where
  constructor MkSuiteInfo
  filter : Filter' f
  global : Global' f

public export
SuiteInfoCommand : Type
SuiteInfoCommand = Done SuiteInfoCommand'

export
TyMap SuiteInfoCommand' where
  tyMap func x = MkSuiteInfo
    (tyMap func x.filter) (tyMap func x.global)

export
TyTraversable SuiteInfoCommand' where
  tyTraverse func x = [| MkSuiteInfo
      (tyTraverse func x.filter) (tyTraverse func x.global)
      |]

export
Show SuiteInfoCommand where
  show i = unwords
    [ "MkSuiteInfo"
    , show i.filter
    , show i.global
    ]

optParseInfo : OptParse (Builder SuiteInfoCommand') SuiteInfoCommand
optParseInfo = [| MkSuiteInfo
  (embed SuiteInfoCommand'.filter (\x => {filter := x}) optParseFilter)
  (embed global (\x => {global := x}) optParseGlobal)
  |]

defaultInfo : Default SuiteInfoCommand'
defaultInfo = MkSuiteInfo
  defaultFilter
  defaultGlobal

export
withGivenGlobal : Default SuiteInfoCommand' -> Default Global' -> Default SuiteInfoCommand'
withGivenGlobal x g = {global := g <+> defaultGlobal} x

export
parseSuiteInfo : Default Global' ->  List String -> ParseResult SuiteInfoCommand
parseSuiteInfo g xs =
  case parse (initBuilder $ defaultInfo `withGivenGlobal` g) optParseInfo xs of
    InvalidMix reason => InvalidMix reason
    InvalidOption ys => InvalidOption $ singleton $ "Unknown option(s): \{show $ toList ys}"
    Done x => maybe (InvalidMix "File is not set") Done $ build x

export
helpSuiteInfo : Help
helpSuiteInfo =
  commandHelp {b = Builder SuiteInfoCommand'}
    ("replica":::["info"]) "suite" "Display information about test suites"
    optParseInfo
    (Just "JSON_TEST_FILE")
