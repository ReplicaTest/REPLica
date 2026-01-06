module Replica.Command.Info.Tag

import Data.String

import Replica.Help
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Option.Types
import Replica.Other.Decorated

public export
record TagInfoCommand' (f : Type -> Type) where
  constructor MkTagInfo
  filter : Filter' f
  global : Global' f

public export
TagInfoCommand : Type
TagInfoCommand = Done TagInfoCommand'

export
TyMap TagInfoCommand' where
  tyMap func x = MkTagInfo
    (tyMap func x.filter) (tyMap func x.global)

export
TyTraversable TagInfoCommand' where
  tyTraverse func x = [| MkTagInfo
      (tyTraverse func x.filter) (tyTraverse func x.global)
      |]

export
Show TagInfoCommand where
  show i = unwords
    [ "MkTagInfo"
    , show i.filter
    , show i.global
    ]

optParseInfo : OptParse (Builder TagInfoCommand') TagInfoCommand
optParseInfo = [| MkTagInfo
  (embed TagInfoCommand'.filter (\x => {filter := x}) optParseFilter)
  (embed global (\x => {global := x}) optParseGlobal)
  |]

defaultInfo : Default TagInfoCommand'
defaultInfo = MkTagInfo
  defaultFilter
  defaultGlobal

export
withGivenGlobal : Default TagInfoCommand' -> Default Global' -> Default TagInfoCommand'
withGivenGlobal x g = {global := g <+> defaultGlobal} x


export
helpTagInfo : Help
helpTagInfo =
  commandHelp {b = Builder TagInfoCommand'}
    ("replica":::["info"]) "tag" "Display information about test tags"
    optParseInfo
    (Just "JSON_TEST_FILE")

export
parseTagInfo : Default Global' ->  List String -> ParseResult TagInfoCommand
parseTagInfo g xs = do
  builder <- parse
    helpTagInfo
    (initBuilder $ defaultInfo `withGivenGlobal` g)
    optParseInfo
    xs
  maybe (InvalidMix "No test file given") Done $ build builder
