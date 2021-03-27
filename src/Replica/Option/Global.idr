module Replica.Option.Global

import Data.List
import Data.String

import Replica.Help
import Replica.Option.Types
import Replica.Other.Decorated

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
record Global' (f : Type -> Type) where
  constructor MkGlobal
  replicaDir : f String
  colour : f Bool
  ascii : f Bool
  logLevel : f (Maybe LogLevel)
  diff : f DiffCommand
  file : f String

public export
Global : Type
Global = Done Global'

export
Show Global where
  show x = unwords
    [ "MkGlobal"
    , show x.replicaDir
    , show x.colour
    , show x.ascii
    , show x.logLevel
    , show x.diff
    , show x.file
    ]

export
TyMap Global' where
  tyMap func x = MkGlobal
    (func x.replicaDir) (func x.colour)
    (func x.ascii) (func x.logLevel)
    (func x.diff) (func x.file)

export
TyTraversable Global' where
  tyTraverse func x =
    [| MkGlobal
    (func x.replicaDir) (func x.colour)
    (func x.ascii) (func x.logLevel)
    (func x.diff) (func x.file)
    |]

replicaDirPart : Part (Builder Global') String
replicaDirPart = inj $ MkOption
  (singleton $ MkMod (singleton "replica-dir") []
      (Right $ MkValue "DIR" Just)
      "set the location of replica store (default: \".replica\")")
  ".replica"
  go
  where
    go : String -> Builder Global' -> Either String (Builder Global')
    go = one replicaDir (\x => record {replicaDir = Right x})
                 (\x, y => "More than one replica dir were given: \{y}, \{x}")

logLevelPart : Part (Builder Global') (Maybe LogLevel)
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
    go : Maybe LogLevel -> Builder Global' -> Either String (Builder Global')
    go = ifSame logLevel (\x => record {logLevel = Right $ x})
                         (const $ const "Contradictory log level")

colourPart : Part (Builder Global') Bool
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
    go : Bool -> Builder Global' -> Either String (Builder Global')
    go = ifSame colour (\x => record {colour = Right $ x})
                       (const $ const "Contradictory colour settings")

asciiPart : Part (Builder Global') Bool
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
      go : Bool -> Builder Global' -> Either String (Builder Global')
      go = ifSame ascii (\x => record {ascii = Right $ x})
                        (const $ const "Contradictory ascii settings")

diffPart : Part (Builder Global') DiffCommand
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
    compose : DiffCommand -> Builder Global' -> Either String (Builder Global')
    compose = one diff (\x => record {diff = Right x})
                  (\x, y => "More than one diff command were given: \{show y}, \{show x}")

export
fileParamPart : Part (Builder Global') String
fileParamPart = inj $ MkParam "JSON_FILE" Just go
  where
    go : String -> Builder Global' -> Either String (Builder Global')
    go = one file
             (\x => record {file = Right x})
             (\x, y => "More than one test file were given: \{y}, \{x}")


export
optParseGlobal : OptParse (Builder Global') Global
optParseGlobal =
  [| MkGlobal
    (liftAp replicaDirPart)
    (liftAp colourPart)
    (liftAp asciiPart)
    (liftAp logLevelPart)
    (liftAp diffPart)
    (liftAp fileParamPart)
  |]

export
defaultGlobal : Default Global'
defaultGlobal =
  MkGlobal
    (defaultPart replicaDirPart)
    (defaultPart colourPart)
    (defaultPart asciiPart)
    (defaultPart logLevelPart)
    (defaultPart diffPart)
    (defaultPart fileParamPart)

export
globalOptionsHelp : List1 Help
globalOptionsHelp = toList1 {ok = ?trustMe}
  $ runApM (\p => partHelp p) optParseGlobal
