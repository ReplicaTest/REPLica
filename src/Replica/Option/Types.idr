module Replica.Option.Types

import Data.List
import public Data.List1
import public Data.List.AtIndex
import Data.Maybe
import public Data.OpenUnion

import Replica.Help
import public Replica.Other.Free

%default total

namespace Experiment

  export
  prefixLongOption : String -> String
  prefixLongOption = ("--" <+>)

  export
  prefixShortOption : Char -> String
  prefixShortOption x = pack ['-',x]

  public export
  record Value a where
    constructor MkValue
    name : String
    parser : String -> Maybe a

  public export
  record Mod a where
    constructor MkMod
    longNames : List1 String
    shortNames : List Char
    param : Either a (Value a)
    description : String

  public export
  record Option b a where
    constructor MkOption
    mods : List1 (Mod a)
    defaultValue : a
    setter : a -> b -> b


namespace Param

  public export
  record Param b a where
    constructor MkParam
    name : String
    parser : String -> Maybe a
    setter : a -> b -> b

namespace Parts

  public export
  Part : Type -> Type -> Type
  Part b a = Union (\p => p b a) [Param, Option]

public export
OptParse : Type -> Type -> Type
OptParse = Ap . Part

namespace Parser

  Parser : (a : Type) -> Type
  Parser a = List String -> Maybe (List String, a)

  modParser : Mod a -> Parser a
  modParser m [] = Nothing
  modParser m (x::xs) = let
    validOption = map prefixLongOption (forget m.longNames)
                  ++ map prefixShortOption m.shortNames
    in do
      guard $ x `elem` validOption
      let Right v = m.param
        | Left r => pure (xs, r)
      case xs of
           [] => Nothing
           (y::ys) => MkPair ys <$> v.parser y

  optionParser : Option b a -> Parser (b -> b)
  optionParser x xs = map x.setter <$> choiceMap (flip modParser xs) x.mods

  partParser : Part b a -> Parser (b -> b)
  partParser x xs = let
    Left x1 = decomp x
      | Right v => case xs of
                        [y] => MkPair [] . v.setter <$> v.parser y
                        _   => Nothing
    in optionParser (decomp0 x1) xs

  export
  parse' : b ->
    OptParse b a ->
    List String ->
    Either (List1 String, b) (List String, b)
  parse' acc o [] = pure ([], acc)
  parse' acc o (x::xs) = let
    Just (xs', f) = runApM (\p => partParser p (x::xs)) o
      | Nothing => Left (x:::xs, acc)
    in parse' (f acc) o $ assert_smaller (x::xs) xs'

  export
  parse : Monoid a => OptParse a b -> List String -> Either String a
  parse xs = bimap (head . fst) snd . parse' neutral xs

namespace Help

  optionName : (long : List String) -> (short : List Char) ->
               (param : Either a (Value b)) -> String
  optionName long short param =
    either (flip const) (\v => (++ " \{v.name}")) param $
    (concat $ intersperse ", " $
       map ("--" ++) long ++ map (\c => pack ['-',c]) short)

  modHelp : Mod a -> Help
  modHelp x = MkHelp (optionName (forget x.longNames) x.shortNames x.param)
                     Nothing x.description [] Nothing

  export
  partHelp : Part b a -> List Help
  partHelp x = let
    Left x1 = decomp x
      | Right v => []
    v = decomp0 x1
    in map modHelp $ forget v.mods

  export
  commandHelp :
    (name : String) -> (description : String) ->
    (global : List1 Help) -> (options : OptParse b c) ->
    (param : Maybe (Param b a)) -> Help
  commandHelp name description global options param = MkHelp
    name
    (Just "replica [GLOBAL_OPTIONS] \{name} [OPTIONS] \{paramExt}")
    description
    ( catMaybes
       [ Just $ MkPair "Global options" global
       , map (MkPair "Specific options") $
           toList1' $ runApM (\p => partHelp p) options ])
    Nothing
    where
      paramExt : String
      paramExt = maybe "" (.name) param
