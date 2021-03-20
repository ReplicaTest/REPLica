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

export
Monoid (InfoAction' List) where
  neutral = MkInfo empty empty

testFilePart : Part String
testFilePart = inj $ MkParam "filename" Just

fileOption : String -> InfoAction' List
fileOption x = record {file = [x]} (neutral {ty = InfoAction' List})

showExpectationPart : Part Bool
showExpectationPart = inj showExpectation
  where
    showExpectation : FlagOption Bool
    showExpectation = MkFlag
      (singleton "expectations") ['e']
      [] []
      "show expectation for each test"
      False
      True

showExpectationOption : Bool -> InfoAction' List
showExpectationOption x =
  record {showExpectation = [x]} (neutral {ty = InfoAction' List})


parseInfoOptions : List String -> Validation (List String) (InfoAction' List)
parseInfoOptions xs =
  either (\x => Error ["Unknown option \{x}"]) Valid
    $ parse [ map {f = Part} showExpectationOption showExpectationPart
            , map {f = Part} fileOption testFilePart
            ] xs

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
helpInfo global = commandHelp "info" "Display information about test suites" global
  (helpPart {a = Bool} showExpectationPart)
  (prj testFilePart)
