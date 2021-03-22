module Replica.Command.Info

import Replica.Help
import Replica.Option.Types
import Replica.Option.Validate
import Replica.Other.Validation

%default total

public export
record InfoAction' (f : Type -> Type) where
  constructor MkInfo
  showExpectation : f Bool
  file : f String

public export
InfoAction : Type
InfoAction = InfoAction' Prelude.id

export
Semigroup (InfoAction' List) where
  (<+>) x y = MkInfo
    (x.showExpectation ++ y.showExpectation)
    (x.file ++ y.file)

infoNeutral : InfoAction' List
infoNeutral = MkInfo empty empty

export
Monoid (InfoAction' List) where
  neutral = infoNeutral

testFilePart : Part (InfoAction' List) String
testFilePart =
  inj $ MkParam "JSON_FILE" Just (\x => record {file $= (x::)})

showExpectationPart : Part (InfoAction' List) Bool
showExpectationPart = inj $ MkOption
      ( singleton
        $ MkMod (singleton "expectations") ['e'] (Left True)
          "show expectation for each test")
      False
      (\b => record {showExpectation = [b]})


optParseInfo : OptParse (InfoAction' List) InfoAction
optParseInfo = [|MkInfo (liftAp showExpectationPart) (liftAp testFilePart)|]

parseInfoOptions : List String -> Validation (List String) (InfoAction' List)
parseInfoOptions xs =
  either (\x => Error ["Unknown option \{x}"]) Valid
  $ parse optParseInfo xs

validateInfoAction : InfoAction' List -> Validation (List String) InfoAction
validateInfoAction nfo =
  [| MkInfo
  (oneValidate showExpectationPart nfo.showExpectation)
  (oneValidate testFilePart nfo.file)
  |]

export
parseInfo : List String -> Validation (List String) InfoAction
parseInfo ("info" :: xs)
  = case parseInfoOptions xs of
         Valid x => validateInfoAction x
         Error e => Error e
parseInfo _ = empty

export
helpInfo : (global : List1 Help) -> Help
helpInfo global =
  commandHelp "info" "Display information about test suites" global
    optParseInfo (prj testFilePart)
