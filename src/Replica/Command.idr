module Replica.Command

import Data.List.AtIndex
import Data.OpenUnion

import Replica.Other.Validation

import public Replica.Command.Info
import public Replica.Command.Run
import public Replica.Command.Help

public export
Actions : Type
Actions = Union Prelude.id [RunAction, InfoAction, Help]

export
parseArgs : List String -> Validation (List String) Actions
parseArgs xs = choiceMap (fromEither . flip apply xs)
  [ map inj . parseRun
  , map inj . parseInfo
  , map inj . parseHelp
  ]
