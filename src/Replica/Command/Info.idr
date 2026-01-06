module Replica.Command.Info

import Data.String

import Replica.Help
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Option.Types
import Replica.Other.Decorated

import Replica.Command.Info.Suite
import Replica.Command.Info.Tag
import Replica.Command.Info.Test

%default total

public export
data InfoCommand' : (f : Type -> Type) -> Type where
  SuiteInfo : SuiteInfoCommand' f -> InfoCommand' f
  TagInfo : TagInfoCommand' f -> InfoCommand' f
  TestInfo : TestInfoCommand' f -> InfoCommand' f

public export
InfoCommand : Type
InfoCommand = Done InfoCommand'

export
TyMap InfoCommand' where
  tyMap func (SuiteInfo x) = SuiteInfo (tyMap func x)
  tyMap func (TagInfo x) = TagInfo (tyMap func x)
  tyMap func (TestInfo x) = TestInfo (tyMap func x)

export
TyTraversable InfoCommand' where
  tyTraverse func (SuiteInfo x) = [| SuiteInfo (tyTraverse func x) |]
  tyTraverse func (TagInfo x) = [| TagInfo (tyTraverse func x) |]
  tyTraverse func (TestInfo x) = [| TestInfo (tyTraverse func x) |]

export
Show InfoCommand where
  show (SuiteInfo i) = unwords [ "SuiteInfo", "(", show i, ")" ]
  show (TagInfo i) = unwords [ "TagInfo", "(", show i, ")" ]
  show (TestInfo i) = unwords [ "TestInfo", "(", show i, ")" ]

export
helpInfo : Help
helpInfo =
  MkHelp
    "info"
    (Just "replica info [TOPIC] [TOPIC_OPTIONS] JSON_TEST_FILE")
    "Get information about a given test file"
    [ ("Topics", helpTestInfo ::: [helpSuiteInfo, helpTagInfo])
    ]
    (Just "Run 'replica help info TOPIC' for more information on a topic.")

export
parseInfo : Default Global' ->  List1 String -> ParseResult InfoCommand
parseInfo g ("info":::xs) = case xs of
  "suite"::xs' => SuiteInfo <$> parseSuiteInfo g xs'
  "tag"::xs' => TagInfo <$> parseTagInfo g xs'
  "test"::xs' => TestInfo <$> parseTestInfo g xs'
  _ => TestInfo <$> parseTestInfo g xs
parseInfo _ xs = InvalidOption Nothing xs
