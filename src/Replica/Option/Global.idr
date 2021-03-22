module Replica.Option.Global

import Data.List
import Data.String

import Replica.Help
import Replica.Option.Types
import Replica.Option.Validate
import Replica.Other.Validation

public export
data LogLevel = Debug | Info | Warning | Critical

export
Show LogLevel where
  show Debug = "Debug"
  show Info = "Info"
  show Warning = "Warning"
  show Critical = "Critical"

levelToNat : LogLevel -> Nat
levelToNat Debug = 0
levelToNat Info = 1
levelToNat Warning = 2
levelToNat Critical = 3

export
Eq LogLevel where
  (==) = (==) `on` levelToNat

export
Ord LogLevel where
  compare = compare `on` levelToNat

public export
record GlobalOption' (f : Type -> Type) where
  constructor MkGlobalOption
  replicaDir : f String
  colour : f Bool
  ascii : f Bool
  logLevel : f (Maybe LogLevel)

public export
GlobalOption : Type
GlobalOption = GlobalOption' id

export
Show GlobalOption where
  show x = unwords
    [ "MkGlobalOption"
    , show x.replicaDir
    , show x.colour
    , show x.ascii
    , show x.logLevel
    ]

export
Semigroup (GlobalOption' List) where
  (<+>) x y =
    MkGlobalOption
      (x.replicaDir <+> y.replicaDir)
      (x.colour <+> y.colour)
      (x.ascii <+> y.ascii)
      (x.logLevel <+> y.logLevel)

export
Monoid (GlobalOption' List) where
  neutral = MkGlobalOption empty empty empty empty

neutralGlobal : GlobalOption' List
neutralGlobal = neutral

replicaDirPart : Part (GlobalOption' List) String
replicaDirPart = inj $ MkOption
  (singleton $ MkMod (singleton "replica-dir") []
      (Right $ MkValue "DIR" Just)
      "set the location of replica store (default: \".replica\")")
  ".replica"
  \x =>record {replicaDir $= (x::)}

logLevelPart : Part (GlobalOption' List) (Maybe LogLevel)
logLevelPart = inj $ MkOption
  (toList1
    [ MkMod (singleton "log") [] (Right logLevelValue)
        "define the log level of the application <none, debug, info, warning, critical> (default: none)"
    , MkMod (singleton "verbose") ['v'] (Left $ Just Info)
        "similar to --log info"
    ])
  Nothing
  \x => record {logLevel $= (x::)}
  where
    logLevelValue : Value (Maybe LogLevel)
    logLevelValue = MkValue "logLevel" (go . toLower)
      where
        go : String -> Maybe (Maybe LogLevel)
        go "none" = Just Nothing
        go "debug" = Just $ Just Debug
        go "info" = Just $ Just Info
        go "warning" = Just $ Just Warning
        go "critical" = Just $ Just Critical
        go _ = Nothing


colourPart : Part (GlobalOption' List) Bool
colourPart = inj $ MkOption
      (toList1
        [ MkMod (toList1 ["color", "colour"]) ['c'] (Left True)
            "activate colour in output (default)"
        , MkMod (toList1 ["no-color", "no-colour"]) [] (Left False)
            "desactivate colour in output"
        ])
      True
      \x => record {colour $= (x::)}


asciiPart : Part (GlobalOption' List) Bool
asciiPart = inj $ MkOption
      (toList1
        [ MkMod (singleton "utf8") [] (Left False)
            "allow emojis in reports (default)"
        , MkMod (singleton "ascii") [] (Left True)
             "use only ascii in reports (unless there are some in your test file)"
        ])
      False
      \x => record {ascii $= (x::)}


optParseGlobal : OptParse (GlobalOption' List) GlobalOption
optParseGlobal =
  [| MkGlobalOption
    (liftAp replicaDirPart)
    (liftAp colourPart)
    (liftAp asciiPart)
    (liftAp logLevelPart)
  |]

parseGlobalOptions : List String -> (List String, GlobalOption' List)
parseGlobalOptions xs =
  either (mapFst forget) id
    $ parse' neutral optParseGlobal xs

validateGeneral : GlobalOption' List -> Validation (List String) GlobalOption
validateGeneral r
  = [| MkGlobalOption
    (oneValidate replicaDirPart r.replicaDir)
    (oneValidate colourPart r.colour)
    (oneValidate asciiPart r.ascii)
    (oneValidate logLevelPart r.logLevel)
    |]

export
parseGlobal : List String -> Validation (List String) (List String, GlobalOption)
parseGlobal xs = let
  (command, opts) = parseGlobalOptions xs
  in map (MkPair command) $ validateGeneral opts

export
globalOptionsHelp : List1 Help
globalOptionsHelp = toList1 {ok = ?trustMe}
  $ runApM (\p => partHelp p) optParseGlobal
