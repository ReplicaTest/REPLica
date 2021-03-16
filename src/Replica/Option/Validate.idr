module Replica.Option.Validate

import Replica.Other.Validation

%default total

export
one : String -> List a -> Validation (List String) a
one _ [z] = Valid z
one x [] = Error ["\{x} is missing"]
one x _ = Error ["\{x} is set several time"]

export
oneWithDefault : String -> a -> List a -> Validation (List String) a
oneWithDefault _ y [] = Valid y
oneWithDefault _ _ [z] = Valid z
oneWithDefault x y _ = Error ["\{x} is set several time"]
