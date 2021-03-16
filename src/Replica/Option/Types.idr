module Replica.Option.Types

import Data.List
import public Data.List1

%default total

namespace Param

  public export
  record Param a where
    constructor MkParam
    name : String
    parser : String -> Maybe a

namespace Option

  export
  prefixLongOption : String -> String
  prefixLongOption = ("--" <+>)

  export
  prefixShortOption : Char -> String
  prefixShortOption x = pack ['-',x]

  public export
  record FlagOption a where
    constructor MkFlag
    changeLong : List1 String
    changeShort : List Char
    enforceLong : List String
    enforceShort : List Char
    description : String
    defaultValue : a
    invert : a -> a

  export
  parseFlagOption : FlagOption a -> List String -> Maybe (a, List String)
  parseFlagOption o (x::xs) = let
    enforceParams = map prefixLongOption o.enforceLong
                  ++ map prefixShortOption o.enforceShort
    changeParams = map prefixLongOption (toList o.changeLong)
                 ++ map prefixShortOption o.changeShort
    in (guard (x `elem` enforceParams) $> (o.defaultValue, xs))
       <+> (guard (x `elem` changeParams) $> (o.invert o.defaultValue, xs))
  parseFlagOption _ _ = Nothing


  public export
  record ParamOption a where
    constructor MkOption
    longName : List1 String
    shortName : List Char
    description : String
    defaultValue : a
    param : (Param a)

  export
  parseParamOption : ParamOption a -> List String -> Maybe (a, List String)
  parseParamOption o (x::y::xs) = let
    validOption = map prefixLongOption (toList o.longName)
                  ++ map prefixShortOption o.shortName
    in do
      guard (x `elem` validOption)
      map (flip MkPair xs) $ o.param.parser y
  parseParamOption _ _ = Nothing
