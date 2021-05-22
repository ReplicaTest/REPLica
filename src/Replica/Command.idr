module Replica.Command

import Data.List
import Data.List1
import Data.List.AtIndex
import Data.OpenUnion

import Replica.Option.Types
import Replica.Other.Decorated
import Replica.Other.Validation

import public Replica.Command.Info
import public Replica.Command.Run
import public Replica.Command.Help

public export
Commands : Type
Commands = Union Prelude.id [RunCommand, InfoCommand, Help]

export
parseArgs : Default Global' -> List1 String -> ParseResult Commands
parseArgs g xs = foldl1 go $ toList1 $ map (flip apply xs)
  [ map inj . parseRun g
  , map inj . parseInfo g
  , map inj . parseHelp
  ]
  where
    go : ParseResult Commands -> ParseResult Commands -> ParseResult Commands
    go (Done x) _ = Done x
    go _ (Done x) = Done x
    go (InvalidMix x) y = InvalidMix x
    go (InvalidOption ys) y = y
