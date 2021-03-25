module Replica.Command.Info

import Replica.Help
import Replica.Option.Types
import Replica.Other.Decorated
import Replica.Other.Validation

%default total

public export
record InfoAction' (f : Type -> Type) where
  constructor MkInfo
  showExpectation : f Bool
  file : f String

public export
InfoAction : Type
InfoAction = Done InfoAction'

export
TyMap InfoAction' where
  tyMap func x = MkInfo (func x.showExpectation) (func x.file)

export
TyTraversable InfoAction' where
  tyTraverse func x = [| MkInfo (func x.showExpectation) (func x.file) |]

testFilePart : Part (Builder InfoAction') String
testFilePart =
  inj $ MkParam "JSON_FILE" Just go
  where
    go : String -> Builder InfoAction' -> Either String (Builder InfoAction')
    go = one file
             (\x => record {file = Right x})
             (\x, y => "More than one test file were given: \{y}, \{x}")

showExpectationPart : Part (Builder InfoAction') Bool
showExpectationPart = inj $ MkOption
      ( singleton
        $ MkMod (singleton "expectations") ['e'] (Left True)
          "show expectation for each test")
      False
      go
  where
    go : Bool -> Builder InfoAction' -> Either String (Builder InfoAction')
    go = ifSame showExpectation
                (\x => record {showExpectation = Right x})
                (const $ const "Contradictory values for expectations")


optParseInfo : OptParse (Builder InfoAction') InfoAction
optParseInfo = [|MkInfo (liftAp showExpectationPart) (liftAp testFilePart)|]

defaultInfo : Default InfoAction'
defaultInfo = MkInfo (defaultPart showExpectationPart) (defaultPart testFilePart)

export
parseInfo : List String -> Either String InfoAction
parseInfo ("info"::xs) =
  either (\x => Left "Unknown option \{x}") buildInfo
  $ parse (initBuilder defaultInfo) optParseInfo xs
  where
    buildInfo : Builder InfoAction' -> Either String InfoAction
    buildInfo = maybe (Left "File is not set") pure . build
parseInfo _ = Left "Not an info action"

export
helpInfo : (global : List1 Help) -> Help
helpInfo global =
  commandHelp "info" "Display information about test suites" global
    optParseInfo (prj testFilePart)
