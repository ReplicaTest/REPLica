module Replica.Option.Types

import Data.List
import public Data.List1
import public Data.List.AtIndex
import Data.Maybe
import public Data.OpenUnion

import Replica.Help
import public Replica.Other.Free

%default total


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

export
Functor Value where
  map func x = MkValue x.name (map func . x.parser)

public export
record Mod a where
  constructor MkMod
  longNames : List1 String
  shortNames : List Char
  param : Either a (Value a)
  description : String

export
Functor Mod where
  map func x = MkMod x.longNames x.shortNames
    (bimap func (map func) x.param)
    x.description

public export
record Option b a where
  constructor MkOption
  mods : List1 (Mod a)
  defaultValue : a
  setter : a -> b -> Either String b

export
embedOption : (c -> b) -> (b -> c -> c) -> Option b a -> Option c a
embedOption f g x = MkOption x.mods x.defaultValue (embed f g x.setter)
  where
    embed : (c -> b) -> (b -> c -> c) -> (a -> b -> Either String b) -> a -> c -> Either String c
    embed unwrap wrap set p w = flip wrap w <$> set p (unwrap w)

namespace Param

  public export
  record Param b a where
    constructor MkParam
    name : String
    parser : String -> Maybe a
    setter : a -> b -> Either String b

  export
  embedParam : (c -> b) -> (b -> c -> c) -> Param b a -> Param c a
  embedParam f g x = MkParam x.name x.parser (embed f g x.setter)
    where
      embed : (c -> b) -> (b -> c -> c) -> (a -> b -> Either String b) -> a -> c -> Either String c
      embed unwrap wrap set p w = flip wrap w <$> set p (unwrap w)

namespace Parts

  public export
  Part : Type -> Type -> Type
  Part b a = Union (\p => p b a) [Param, Option]

  export
  embedPart : (c -> b) -> (b -> c -> c) -> Part b a -> Part c a
  embedPart get set x = let
    Left x1 = decomp x
      | Right v => inj $ embedParam get set v
    v = decomp0 x1
    in inj $ embedOption get set v

public export
OptParse : Type -> Type -> Type
OptParse = Ap . Part

export
embed : (c -> b) -> (b -> c -> c) -> OptParse b a -> OptParse c a
embed get set (Pure x) = Pure x
embed get set (MkAp x y) = MkAp (embedPart get set x) $ embed get set y

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

  optionParser : Option b a -> Parser (b -> Either String b)
  optionParser x xs = map x.setter <$> choiceMap (flip modParser xs) x.mods

  partParser : Part b a -> Parser (b -> Either String b)
  partParser x xs = let
    Left x1 = decomp x
      | Right v => case xs of
                        [y] => MkPair [] . v.setter <$> v.parser y
                        _   => Nothing
    in optionParser (decomp0 x1) xs

  public export
  data ParseResult a
    = InvalidOption (List1 String)
    | InvalidMix String -- reason
    | Done a

  export
  Functor ParseResult where
    map func (InvalidOption xs) = InvalidOption xs
    map func (InvalidMix x) = InvalidMix x
    map func (Done x) = Done (func x)

  export
  parse : a ->
    OptParse a b ->
    List String ->
    ParseResult a
  parse acc o [] = Done acc
  parse acc o (x::xs) = let
    Just (xs', f) = runApM (\p => partParser p (x::xs)) o
      | Nothing => InvalidOption (x:::xs)
    in either
         InvalidMix
         (\y => parse y o $ assert_smaller (x::xs) xs')
         (f acc)

namespace Default

  defaultOption : Option b a -> Maybe a
  defaultOption = Just . defaultValue

  defaultParam : Param b a -> Maybe a
  defaultParam = const Nothing

  export
  defaultPart : Part b a -> Maybe a
  defaultPart x = let
   Left x1 = decomp x
     | Right v => defaultParam v
   v = decomp0 x1
   in defaultOption v

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
    (options : OptParse b c) ->
    (param : Maybe String) -> Help
  commandHelp name description options param = MkHelp
    name
    (Just "replica \{name} [OPTIONS]\{paramExt param}")
    description
    ( catMaybes
       [ map (MkPair "Options") $
           toList1' $ reverse $ runApM (\p => partHelp p) options
       ])
    Nothing
    where
      paramExt : Maybe String -> String
      paramExt = maybe "" (" "<+>)
