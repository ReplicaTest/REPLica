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
Actions : Type
Actions = Union Prelude.id [RunAction, InfoAction, Help]

export
parseArgs : List1 String -> ParseResult Actions
parseArgs xs = foldl1 go $ toList1 $ map (flip apply xs)
  [ map inj . parseRun
  , map inj . parseInfo
  , map inj . parseHelp
  ]
  where
    go : ParseResult Actions -> ParseResult Actions -> ParseResult Actions
    go (Done x) _ = Done x
    go _ (Done x) = Done x
    go (InvalidMix x) y = InvalidMix x
    go (InvalidOption ys) y = y
