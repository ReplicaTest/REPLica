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

replicaDirPart : Part String
replicaDirPart = inj $ MkOption
  (singleton "replica-dir")
  []
  "set the location of replica store (default: \".replica\")"
  ".replica"
  (MkParam "dirName" Just)

replicaDirOption : String -> GlobalOption' List
replicaDirOption x =
  record {replicaDir = [x]} (neutral {ty = GlobalOption' List})

logLevelPart : Part (Maybe LogLevel)
logLevelPart = inj $ MkOption
  (singleton "log")
  []
  "define the log level of the application <none, debug, info, warning, critical> (default: none)"
  Nothing
  logLevelParam
  where
    logLevelParam : Param (Maybe LogLevel)
    logLevelParam = MkParam "logLevel" (go . toLower)
      where
        go : String -> Maybe (Maybe LogLevel)
        go "none" = Just Nothing
        go "debug" = Just $ Just Debug
        go "info" = Just $ Just Info
        go "warning" = Just $ Just Warning
        go "critical" = Just $ Just Critical
        go _ = Nothing


verbosePart : Part (Maybe LogLevel)
verbosePart = inj $ MkFlag
  (singleton "verbose") ['v']
  [] []
  "similar to --log info"
  Nothing
  (Just Info)

logLevelOption : Maybe LogLevel -> GlobalOption' List
logLevelOption x = record {logLevel = [x]} (neutral {ty = GlobalOption' List})

colourPart : Part Bool
colourPart = inj colour
  where
    colour : FlagOption Bool
    colour = MkFlag
      ("no-color" ::: ["no-colour"]) []
      ["color", "colour"] ['c']
      "desactivate colour in output"
      True
      False

colourOption : Bool -> GlobalOption' List
colourOption x = record {colour = [x]} (neutral {ty = GlobalOption' List})


asciiPart : Part Bool
asciiPart = inj ascii
  where
    ascii : FlagOption Bool
    ascii = MkFlag
      (singleton "ascii") []
      ["utf8"] []
      "use only ascii (no emoji) in output (unless there are some in your test file)"
      False
      True

asciiOption : Bool -> GlobalOption' List
asciiOption x = record {ascii = [x]} (neutral {ty = GlobalOption' List})


parseGlobalOptions : List String -> (List String, GlobalOption' List)
parseGlobalOptions xs =
  either (mapFst forget) id
    $ parseFragment
        [ map {f = Part} replicaDirOption replicaDirPart
        , map {f = Part} logLevelOption logLevelPart
        , map {f = Part} logLevelOption verbosePart
        , map {f = Part} colourOption colourPart
        , map {f = Part} asciiOption asciiPart
        ] xs

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
  (  (helpPart {a = String } replicaDirPart)
  ++ (helpPart {a = Maybe LogLevel} logLevelPart)
  ++ (helpPart {a = Maybe LogLevel} verbosePart)
  ++ (helpPart {a = Bool} colourPart)
  ++ (helpPart {a = Bool} asciiPart)
  )
