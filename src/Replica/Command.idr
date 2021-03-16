module Replica.Command

import Data.List.AtIndex
import Data.OpenUnion

import Replica.Other.Validation

import public Replica.Command.Info
import public Replica.Command.Run

public export
Actions : Type
Actions = Union Prelude.id [RunAction, InfoAction]

export
parseArgs : List String -> Validation (List String) Actions
parseArgs xs = choiceMap (flip apply xs)
  [ map inj . parseRun
  , map inj . parseInfo
  ]
