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
import public Replica.Command.Set
import public Replica.Command.New
import public Replica.Command.Help
import public Replica.Command.Version

public export
Commands : Type
Commands = Union [RunCommand, InfoCommand, SetCommand, NewCommand, Help, Version]

export
parseArgs : Default Global' -> List1 String -> ParseResult Commands
parseArgs g xs = foldl1 go $ InvalidOption (pure help) xs ::: map (flip apply xs)
  [ map (inj 0 Z) . parseRun g
  , map (inj 1 (S Z)) . parseInfo g
  , map (inj 2 (S $ S Z)) . parseSet
  , map (inj 3 (S $ S $ S Z)) . parseNew
  , map (inj 4 (S $ S $ S $ S Z)) . parseHelp
  , map (inj 5 (S $ S $ S $ S $ S Z)) . parseVersion
  ]
  where
    go : ParseResult Commands -> ParseResult Commands -> ParseResult Commands
    go (Done x) _ = Done x
    go _ (Done x) = Done x
    go x@(InvalidMix _) y = x
    go x y@(InvalidMix _) = y
    go x@(InvalidOption _ xs) y@(InvalidOption _ ys) =
      if length xs <= length ys then x else y
