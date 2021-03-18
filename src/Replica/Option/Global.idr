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
  logLevel : f (Maybe LogLevel)

public export
GlobalOption : Type
GlobalOption = GlobalOption' id

export
Show GlobalOption where
  show x = "MkGlobalOption \{show x.replicaDir} \{show x.logLevel}"

export
Semigroup (GlobalOption' List) where
  (<+>) x y =
    MkGlobalOption
      (x.replicaDir <+> y.replicaDir)
      (x.logLevel <+> y.logLevel)

export
Monoid (GlobalOption' List) where
  neutral = MkGlobalOption empty empty

replicaDir : ParamOption String
replicaDir = MkOption
  (singleton "replica-dir")
  []
  "set the location of replica store"
  ".replica"
  (MkParam "dirName" Just)

replicaDirOption : String -> GlobalOption' List
replicaDirOption x =
  record {replicaDir = [x]} (neutral {ty = GlobalOption' List})

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

logLevel : ParamOption (Maybe LogLevel)
logLevel = MkOption
  (singleton "log")
  []
  "define the log level of the application <none, debug, info, warning, critical>"
  Nothing
  logLevelParam

verbose : FlagOption (Maybe LogLevel)
verbose = MkFlag
  (singleton "verbose") ['v']
  [] []
  "similar to --log info"
  Nothing
  (Just Info)

logLevelOption : Maybe LogLevel -> GlobalOption' List
logLevelOption x =
  record {logLevel = [x]} (neutral {ty = GlobalOption' List})

parseGlobalOptions : List String -> (List String, GlobalOption' List)
parseGlobalOptions xs =
  either (mapFst forget) id
    $ parseFragment
        [ map {f = Part} replicaDirOption $ inj replicaDir
        , map {f = Part} logLevelOption $ inj logLevel
        , map {f = Part} logLevelOption $ inj verbose
        ] xs

validateGeneral : GlobalOption' List -> Validation (List String) GlobalOption
validateGeneral r
  = [| MkGlobalOption
    (oneValidate (inj replicaDir) r.replicaDir)
    (oneValidate (inj logLevel) r.logLevel)
    |]

export
parseGlobal : List String -> Validation (List String) (List String, GlobalOption)
parseGlobal xs = let
  (command, opts) = parseGlobalOptions xs
  in map (MkPair command) $ validateGeneral opts

export
globalOptionsHelp : List1 Help
globalOptionsHelp = toList1 {ok = ?trustMe}
  (  (helpPart $ inj replicaDir)
  ++ (helpPart $ inj logLevel)
  ++ (helpPart $ inj verbose)
  )
