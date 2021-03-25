module Replica.Option.Global

import Data.List
import Data.String

import Replica.Help
import Replica.Option.Types
import public Replica.Other.Decorated
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

public export
data DiffCommand
  = Native
  | Diff
  | GitDiff
  | Custom String

export
Show DiffCommand where
  show Native = "Native"
  show Diff = "Diff"
  show GitDiff = "GitDiff"
  show (Custom x) = "Custom \{show x}"

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
  diff : f DiffCommand

public export
GlobalOption : Type
GlobalOption = Done GlobalOption'

export
Show GlobalOption where
  show x = unwords
    [ "MkGlobalOption"
    , show x.replicaDir
    , show x.colour
    , show x.ascii
    , show x.logLevel
    , show x.diff
    ]

export
TyMap GlobalOption' where
  tyMap func x = MkGlobalOption
    (func x.replicaDir) (func x.colour)
    (func x.ascii) (func x.logLevel)
    (func x.diff)

TyTraversable GlobalOption' where
  tyTraverse func x =
    [| MkGlobalOption
    (func x.replicaDir) (func x.colour)
    (func x.ascii) (func x.logLevel)
    (func x.diff)
    |]

replicaDirPart : Part (Builder GlobalOption') String
replicaDirPart = inj $ MkOption
  (singleton $ MkMod (singleton "replica-dir") []
      (Right $ MkValue "DIR" Just)
      "set the location of replica store (default: \".replica\")")
  ".replica"
  go
  where
    go : String -> Builder GlobalOption' -> Either String (Builder GlobalOption')
    go = one replicaDir (\x => record {replicaDir = Right x})
                 (\x, y => "More than one replica dir were given: \{y}, \{x}")

logLevelPart : Part (Builder GlobalOption') (Maybe LogLevel)
logLevelPart = inj $ MkOption
  (toList1
    [ MkMod (singleton "log") [] (Right logLevelValue)
        #"""
        define the log level of the application
        available values: <none, debug, info, warning, critical> (default: none)
        """#
    , MkMod (singleton "verbose") ['v'] (Left $ Just Info)
        "similar to --log info"
    ])
  Nothing
  go
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
    go : Maybe LogLevel -> Builder GlobalOption' -> Either String (Builder GlobalOption')
    go = ifSame logLevel (\x => record {logLevel = Right $ x})
                         (const $ const "Contradictory log level")

colourPart : Part (Builder GlobalOption') Bool
colourPart = inj $ MkOption
      (toList1
        [ MkMod (toList1 ["color", "colour"]) ['c'] (Left True)
            "activate colour in output (default)"
        , MkMod (toList1 ["no-color", "no-colour"]) [] (Left False)
            "desactivate colour in output"
        ])
      True
      go
    where
    go : Bool -> Builder GlobalOption' -> Either String (Builder GlobalOption')
    go = ifSame colour (\x => record {colour = Right $ x})
                       (const $ const "Contradictory colour settings")

asciiPart : Part (Builder GlobalOption') Bool
asciiPart = inj $ MkOption
      (toList1
        [ MkMod (singleton "utf8") [] (Left False)
            "allow emojis in reports (default)"
        , MkMod (singleton "ascii") [] (Left True)
             "use only ascii in reports (unless there are some in your test file)"
        ])
      False
      go
      where
      go : Bool -> Builder GlobalOption' -> Either String (Builder GlobalOption')
      go = ifSame ascii (\x => record {ascii = Right $ x})
                        (const $ const "Contradictory ascii settings")

diffPart : Part (Builder GlobalOption') DiffCommand
diffPart = inj $ MkOption
  (toList1 [ MkMod (singleton "diff") ['d'] (Right parseDiff)
     #"""
     diff command use to display difference between the given and the golden one
     available values: <git|diff|native|custom_command> (default : native)
     """#])
  Native
  compose
  where
    go : String -> DiffCommand
    go "native" = Native
    go "git" = GitDiff
    go "diff" = Diff
    go x = Custom x
    parseDiff : Value DiffCommand
    parseDiff = MkValue "CMD" (Just . go . toLower)
    compose : DiffCommand -> Builder GlobalOption' -> Either String (Builder GlobalOption')
    compose = one diff (\x => record {diff = Right x})
                  (\x, y => "More than one diff command were given: \{show y}, \{show x}")

optParseGlobal : OptParse (Builder GlobalOption') GlobalOption
optParseGlobal =
  [| MkGlobalOption
    (liftAp replicaDirPart)
    (liftAp colourPart)
    (liftAp asciiPart)
    (liftAp logLevelPart)
    (liftAp diffPart)
  |]

defaultGlobal : Default GlobalOption'
defaultGlobal =
  MkGlobalOption
    (defaultPart replicaDirPart)
    (defaultPart colourPart)
    (defaultPart asciiPart)
    (defaultPart logLevelPart)
    (defaultPart diffPart)

export
parseGlobal : List String -> Either String (List String, GlobalOption)
parseGlobal xs = case parse' (initBuilder defaultGlobal) optParseGlobal xs of
                             InvalidOption ys x => MkPair (forget ys) <$> buildGlobal x
                             InvalidMix x => Left x
                             Done x => MkPair [] <$> buildGlobal x
  where
    buildGlobal : Builder GlobalOption' -> Either String GlobalOption
    buildGlobal x = maybe (Left "Missing mandatory parameter") Right $ build x

export
globalOptionsHelp : List1 Help
globalOptionsHelp = toList1 {ok = ?trustMe}
  $ runApM (\p => partHelp p) optParseGlobal
