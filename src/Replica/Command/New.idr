module Replica.Command.New

import Data.List
import Data.List1
import Data.String
import System.Path

import Replica.Help
import public Replica.Option.Types
import Replica.Other.Decorated

public export
data FileFormat = JSON | Dhall

export
Show FileFormat where
  show JSON = "JSON"
  show Dhall = "Dhall"

public export
record NewCommand' (f : Type -> Type) where
  constructor MkNewCommand
  format : f FileFormat
  includeSample : f Bool
  file : f String

public export
NewCommand : Type
NewCommand = Done NewCommand'

TyMap NewCommand' where
  tyMap func x =
    MkNewCommand
      (func x.format)
      (func x.includeSample)
      (func x.file)

TyTraversable NewCommand' where
  tyTraverse func x = [|
    MkNewCommand
      (func x.format)
      (func x.includeSample)
      (func x.file)
      |]

Show NewCommand where
  show x = unwords
    [ "MkNewCommand"
    , show x.format
    , show x.includeSample
    , show x.file
    ]

formatPart : Part (Builder NewCommand') FileFormat
formatPart = inj $ MkOption
  (singleton $ MkMod (singleton "format") ['f']
    (Right $ MkValue "FORMAT" (parseFormat . toLower))
    "format of the file to create (json|dhall)")
  Dhall
  go
  where
    parseFormat : String -> Maybe FileFormat
    parseFormat "json" = Just JSON
    parseFormat "dhall"= Just Dhall
    parseFormat x = Nothing
    go : FileFormat -> Builder NewCommand' -> Either String (Builder NewCommand')
    go = one format
             (\x => {format := Right x})
             (\x, y => "More than one format given: \{show y}, \{show x}")

includeSamplePart : Part (Builder NewCommand') Bool
includeSamplePart = inj $ MkOption (toList1
      [ MkMod (singleton "includeSample") ['s'] (Left True)
            "include a sample test"
      , MkMod (singleton "noSample") ['S'] (Left False)
            "no sample test"
      ])
      True
      go
  where
    go : Bool -> Builder NewCommand' -> Either String (Builder NewCommand')
    go = ifSame includeSample
                (\x => {includeSample := Right x})
                (const $ const "Contradictory values for includeSample")

fileParamPart : Part (Builder NewCommand') String
fileParamPart = inj $ MkParam1 "NEW_TEST_FILE" Just go
  where
    checkFileType : String -> Maybe FileFormat
    checkFileType "json" = Just JSON
    checkFileType "dhall" = Just Dhall
    checkFileType x = Nothing
    setFile : String -> Builder NewCommand' -> Builder NewCommand'
    setFile f = {file := Right f}
    setFileAndFormat : String -> FileFormat -> Builder NewCommand' -> Builder NewCommand'
    setFileAndFormat f fmt = {file := Right f, format := Right fmt}
    go : String -> Builder NewCommand' -> Either String (Builder NewCommand')
    go = one file
             (\x, cmd => case cmd.format of
                 Left _ => maybe
                   (setFile x cmd)
                   (\fmt => setFileAndFormat x fmt cmd)
                   (checkFileType =<< (toLower <$> extension x))
                 Right _ => setFile x cmd
             )
             (const $ const $"Can't write more than one test file")

export
optParseNew : OptParse (Builder NewCommand') NewCommand
optParseNew =
  [| MkNewCommand
     (liftAp formatPart)
     (liftAp includeSamplePart)
     (liftAp fileParamPart)
  |]

export
defaultNew : Default NewCommand'
defaultNew =
  MkNewCommand
    (defaultPart formatPart)
    (defaultPart includeSamplePart)
    (defaultPart fileParamPart)

export
parseNew : List1 String -> ParseResult NewCommand
parseNew ("new":::xs) = do
  case parse (initBuilder defaultNew) optParseNew xs of
         InvalidMix reason => InvalidMix reason
         InvalidOption ys  => InvalidOption $ singleton $ "Unknown option(s): \{show $ toList ys}"
         Done builder      => maybe (InvalidMix "No test file given") Done $ build builder
parseNew xs = InvalidOption xs

export
helpNew : Help
helpNew = commandHelp {b = Builder NewCommand'}
  "new" "Create test files"
  optParseNew
  (Just "NEW_TEST_FILE")
