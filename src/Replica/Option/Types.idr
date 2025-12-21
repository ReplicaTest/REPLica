||| Define types for CLI options
module Replica.Option.Types

import Data.List
import public Data.List1
import public Data.List.AtIndex
import Data.Maybe
import Data.String
import public Data.OpenUnion

import Replica.Help
import public Replica.Other.Free

%default total


||| Transform a string into a CLI long option
export
prefixLongOption : (optionName : String) -> String
prefixLongOption = ("--" <+>)

||| Transform a char into a CLI short option
export
prefixShortOption : (optionChar : Char) -> String
prefixShortOption x = pack ['-',x]

||| Name and parsing of a CLI option
public export
record Value a where
  constructor MkValue
  name : String
  parser : String -> Maybe a

export
Functor Value where
  map func x = MkValue x.name (map func . x.parser)

||| Describe a modifiers of an option
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

||| An option has a list of modifiers, a default value and a setter
public export
record Option b a where
  constructor MkOption
  mods : List1 (Mod a)
  defaultValue : a
  setter : a -> b -> Either String b

||| Reuse an existing option in a different context
export
embedOption : (c -> b) -> (b -> c -> c) -> Option b a -> Option c a
embedOption f g x = MkOption x.mods x.defaultValue (embed f g x.setter)
  where
    embed : (c -> b) -> (b -> c -> c) -> (a -> b -> Either String b) -> a -> c -> Either String c
    embed unwrap wrap set p w = flip wrap w <$> set p (unwrap w)

namespace Param

  ||| A `Param` is the description of a parameter
  public export
  record Param b a where
    constructor MkParam
    name : String
    parser : List String -> Maybe a
    setter : a -> b -> Either String b

  export
  MkParam1 : (name : String) -> (parser : String -> Maybe a) ->
             (setter : a -> b -> Either String b) ->
             Param b a
  MkParam1 name parser setter = MkParam name (go parser) setter
    where
      go : (String -> Maybe a) -> List String -> Maybe a
      go f [x] = f x
      go f _   = Nothing

  export
  embedParam : (c -> b) -> (b -> c -> c) -> Param b a -> Param c a
  embedParam f g x = MkParam x.name x.parser (embed f g x.setter)
    where
      embed : (c -> b) -> (b -> c -> c) -> (a -> b -> Either String b) -> a -> c -> Either String c
      embed unwrap wrap set p w = flip wrap w <$> set p (unwrap w)

namespace Parts

  ||| A `Part` can be a Param or an option
  ||| It's used to list indifferently options and parameters as a part of a command.
  public export
  Part : Type -> Type -> Type
  Part b a = UnionT (\p => p b a) [Param, Option]


  ||| embed a `Param`
  export
  paramPart : Param b a -> Part b a
  paramPart = inj 0 Z

  ||| embed an `Option`
  export
  optionPart : Option b a -> Part b a
  optionPart = inj 1 (S Z)

  ||| Reuse a generic `Part` in a specific setting
  export
  embedPart : (c -> b) -> (b -> c -> c) -> Part b a -> Part c a
  embedPart get set x = let
    Left x1 = decomp x
      | Right v => paramPart $ embedParam get set v
    v = decomp0 x1
    in optionPart $ embedOption get set v

||| A Free applicative of Parts
public export
OptParse : Type -> Type -> Type
OptParse = Ap . Part

||| Reuse a generic `OptParse` in a specific setting
export
embed : (c -> b) -> (b -> c -> c) -> OptParse b a -> OptParse c a
embed get set (Pure x) = Pure x
embed get set (MkAp x y) = MkAp (embedPart get set x) $ embed get set y

namespace Parser

  ||| Generic parser type
  Parser : (a : Type) -> Type
  Parser a = List String -> Maybe (List String, a)

  ||| Parse a` `Mod`
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

  ||| Parse a whole `Option`
  optionParser : Option b a -> Parser (b -> Either String b)
  optionParser x xs = map x.setter <$> choiceMap (flip modParser xs) x.mods

  ||| Parse a `Part`
  partParser : Part b a -> Parser (b -> Either String b)
  partParser x xs = let
    Left x1 = decomp x
      | Right v => MkPair [] . v.setter <$> v.parser xs
    in optionParser (decomp0 x1) xs

  ||| Explain why a CLI call is invalid
  public export
  data ParseResult a
    = InvalidOption (Maybe Help) (List1 String)
    | InvalidMix String -- reason
    | Done a

  ||| Explain why a specific option is invalid
  public export
  data ParsingFailure : ParseResult a -> Type where
    OptionFailure : ParsingFailure (InvalidOption help xs)
    MixFailure : ParsingFailure (InvalidMix reason)

  export
  Semigroup (ParseResult a) where
    Done x <+> _ = Done x
    _ <+> y = y

  export
  Functor ParseResult where
    map func (InvalidOption help xs) = InvalidOption help xs
    map func (InvalidMix reason) = InvalidMix reason
    map func (Done x) = Done (func x)

  export
  Applicative ParseResult where
    pure = Done
    InvalidOption h xs <*> _ = InvalidOption h xs
    InvalidMix reason <*> _ = InvalidMix reason
    Done f <*> InvalidOption h xs = InvalidOption h xs
    Done f <*> InvalidMix reason = InvalidMix reason
    Done f <*> Done x = Done $ f x

  export
  Monad ParseResult where
    InvalidOption h xs >>= f = InvalidOption h xs
    InvalidMix reason >>= f = InvalidMix reason
    Done x >>= f = f x

  ||| Parse a CLI call
  export
  parse :
    Help ->
    a ->
    OptParse a b ->
    List String ->
    ParseResult a
  parse help acc o [] = Done acc
  parse help acc o (x::xs) = let
    Just (xs', f) = runApM (\p => partParser p (x::xs)) o
      | Nothing => case parse help acc o xs of
          InvalidOption h xs' => InvalidOption h $ x:::forget xs'
          _ => InvalidOption (pure help) $ singleton x
    in either
         InvalidMix
         (\acc' => parse help acc' o $ assert_smaller (x::xs) xs')
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
     | Right param => defaultParam param
   option = decomp0 x1
   in defaultOption option

namespace Help

  optionName : (long : List String) -> (short : List Char) ->
               (param : Either a (Value b)) -> String
  optionName long short param =
    either (flip const) (\v => (++ " \{v.name}")) param $
    (concat $ intersperse ", " $
       map prefixLongOption long ++ map prefixShortOption short)

  modHelp : Mod a -> Help
  modHelp x = MkHelp
    (optionName (forget x.longNames) x.shortNames x.param)
    Nothing
    x.description
    []
    Nothing

  export
  partHelp : Part b a -> List Help
  partHelp x = let
    Left x1 = decomp x
      | Right v => []
    v = decomp0 x1
    in map modHelp $ forget v.mods

  export
  commandHelp :
    (parents : List1 String) ->
    (name : String) -> (description : String) ->
    (options : OptParse b c) ->
    (param : Maybe String) -> Help
  commandHelp parents name description options param = MkHelp
    name
    (Just "\{unwords $ forget parents} \{name} [OPTIONS]\{paramExt param}")
    description
    ( catMaybes
       [ map (MkPair "Options") $
           toList1' $ reverse $ runApM (\p => partHelp p) options
       ])
    Nothing
    where
      paramExt : Maybe String -> String
      paramExt = maybe "" (" "<+>)
