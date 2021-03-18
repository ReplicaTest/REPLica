module Replica.Option.Validate

import Replica.Other.Validation
import Replica.Option.Types

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

export
oneValidate : Part a -> List a -> Validation (List String) a
oneValidate x =
  let
    Left x1 = decomp x
      | Right v => one v.name
    Left x2 = decomp x1
     | Right v => oneWithDefault v.changeLong.head v.defaultValue
    v = decomp0 x2
  in oneWithDefault v.longName.head v.defaultValue
