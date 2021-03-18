module Replica.Option.Types

import Data.List
import public Data.List1
import public Data.List.AtIndex
import Data.Maybe
import public Data.OpenUnion

import Replica.Help

%default total

namespace Param

  public export
  record Param a where
    constructor MkParam
    name : String
    parser : String -> Maybe a

  export
  Functor Param where
    map func = record {parser $= (map func .)}

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
    negatedValue : a

  export
  Functor FlagOption where
    map func = record {negatedValue $= func, defaultValue $= func}

  export
  parseFlagOption : FlagOption a -> List String -> Maybe (List String, a)
  parseFlagOption o (x::xs) = let
    enforceParams = map prefixLongOption o.enforceLong
                  ++ map prefixShortOption o.enforceShort
    changeParams = map prefixLongOption (toList o.changeLong)
                 ++ map prefixShortOption o.changeShort
    in (guard (x `elem` enforceParams) $> (xs, o.defaultValue))
       <+> (guard (x `elem` changeParams) $> (xs, o.negatedValue))
  parseFlagOption _ _ = Nothing


  public export
  record ParamOption a where
    constructor MkOption
    longName : List1 String
    shortName : List Char
    description : String
    defaultValue : a
    param : Param a

  export
  Functor ParamOption where
    map func = record {param $= map func, defaultValue $= func}

  export
  parseParamOption : ParamOption a -> List String -> Maybe (List String, a)
  parseParamOption o (x::y::xs) = let
    validOption = map prefixLongOption (toList o.longName)
                  ++ map prefixShortOption o.shortName
    in do
      guard (x `elem` validOption)
      map (MkPair xs) $ o.param.parser y
  parseParamOption _ _ = Nothing

namespace Parts

  public export
  Part : Type -> Type
  Part a = Union (flip apply a) [Param, FlagOption, ParamOption]

  export
  Functor Part where
    map func x = let
      Left x1 = decomp x
        | Right v => inj $ map func v
      Left x2 = decomp x1
        | Right v => inj $ map func v
      in inj $ map func (decomp0 x2)

namespace Parser

  record Parser (a : Type) where
    constructor MkParser
    parser : List String -> Maybe (List String, a)

  Functor Parser where
    map func x = MkParser $ map (map func) . x.parser

  Applicative Parser where
    pure x = MkParser \xs => Just (xs, x)

  Alternative Parser where
    empty = MkParser $ const Nothing
    (<|>) (MkParser x) (MkParser y) = MkParser \xs => x xs <|> y xs

  toParser : Part a -> Parser a
  toParser x = let
    Left x1 = decomp x
      | Right v => MkParser \xs =>
          case xs of
               [str] => map (\res => ([], res)) $ v.parser str
               _ => Nothing
    Left x2 = decomp x1
      | Right v => MkParser $ parseFlagOption v
    x3 = decomp0 x2
    in MkParser $ parseParamOption x3

  export
  parse' : Semigroup a =>
    a ->
    List (List String -> Maybe (List String, a)) ->
    List String ->
    Either (List1 String, a) (List String, a)
  parse' x xs [] = pure ([], x)
  parse' x xs (y::ys) = let
    Just (ys', x') = choiceMap (flip apply (y::ys)) xs
      | Nothing => Left (y:::ys,x)
    in parse' (x<+>x') xs $ assert_smaller (y::ys) ys'

  export
  parseFragment : Monoid a => List (Part a) -> List String ->
    Either (List1 String, a) (List String, a)
  parseFragment = parse' neutral . map (parser . toParser)

  export
  parse : Monoid a => List (Part a) -> List String -> Either String a
  parse xs = bimap (head . fst) snd . parseFragment xs

namespace Help

  optionName : (long : List String) -> (short : List Char) -> String
  optionName long short = concat $ intersperse ", " $
    map ("--" ++) long ++ map (\c => pack ['-',c]) short

  export
  helpPart : Part a -> List Help
  helpPart x = let
    Left x1 = decomp x
      | Right v => []
    Left x2 = decomp x1
      | Right v => catMaybes
          [ Just $ MkHelp (optionName (forget v.changeLong) v.changeShort)
              Nothing v.description [] Nothing
          , let nm = optionName v.enforceLong v.enforceShort
            in guard (nm /= "") $> MkHelp nm Nothing "  do the opposite" [] Nothing
          ]
    v = decomp0 x2
    in [ MkHelp "\{optionName (forget v.longName) v.shortName} <\{v.param.name}>"
           Nothing v.description [] Nothing ]

  export
  commandHelp :
    (name : String) -> (description : String) ->
    (global : List1 Help) -> (options : List Help) ->
    (param : Maybe (Param a)) -> Help
  commandHelp name description global options param = MkHelp
    name
    (Just "replica [GLOBAL_OPTIONS] \{name} [OPTIONS]\{paramExt}")
    description
    ( catMaybes
       [ Just $ MkPair "Global options" global
       , map (MkPair "Specific options") $ toList1' options
       ])
    Nothing
    where
      paramExt : String
      paramExt = maybe "" (.name) param
