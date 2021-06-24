module Replica.Command.Set

import Data.List
import Data.String

import Replica.Help
import Replica.Option.Parse
import Replica.Option.Types
import public Replica.Option.Filter
import public Replica.Option.Global
import Replica.Other.Decorated

import Language.JSON

%default total

public export
data TargetConfig = Local | Global

export
Show TargetConfig where
  show Local = "Local"
  show Global = "Global"

export
Eq TargetConfig where
  Local == Local = True
  Global == Global = True
  _ == _ = False

public export
record Setter where
  constructor MkSetter
  key: String
  value: JSON

export
Show Setter where
  show (MkSetter key value) =
    "MkSetter \{show key} \{show value}"

public export
record SetCommand' (f : Type -> Type) where
  constructor MkSetCommand
  target : f TargetConfig
  setter : f Setter

public export
SetCommand : Type
SetCommand = Done SetCommand'

TyMap SetCommand' where
  tyMap func x =
    MkSetCommand (func x.target) (func x.setter)

TyTraversable SetCommand' where
  tyTraverse func x =
    [| MkSetCommand (func x.target) (func x.setter) |]

export
Show SetCommand where
  show x = "MkSetCommand \{show x.target} (\{show x.setter})"

targetPart : Part (Builder SetCommand') TargetConfig
targetPart = inj $ MkOption
  (toList1
    [ MkMod (singleton "local") ['l'] (Left Local)
      "Set a local config value (in `./.replica.json`) (default)"
    , MkMod (singleton "global") ['g'] (Left Global)
      "Set a global config value (in `$HOME/.replica.json`)"
    ])
  Local
  go
  where
    go : TargetConfig -> Builder SetCommand' -> Either String (Builder SetCommand')
    go = ifSame target (\x => record {target = Right $ x})
                       (const $ const "Contradictory target")

setterPart : Part (Builder SetCommand') Setter
setterPart = inj $ MkParam1
  "KEY=VALUE"
  parseKV
  go
  where
    go : Setter -> Builder SetCommand' -> Either String (Builder SetCommand')
    go s x = Right $ record {setter = Right $ s} x

    buildSetter : ConfigValue -> (String, String) -> Maybe Setter
    buildSetter x = map (uncurry MkSetter) . jsonFor x

    validateKV : String -> String -> Maybe Setter
    validateKV x y with (strM x, strM y)
      validateKV "" y | (StrNil, w) = Nothing
      validateKV x (prim__strCons '=' t) | (w, StrCons '=' t) =
        concatMap (flip buildSetter (x, t)) configValues
      validateKV x y | (w, z) = Nothing

    parseKV : String -> Maybe Setter
    parseKV x = uncurry validateKV $ break (== '=') x

optParseSet : OptParse (Builder SetCommand') SetCommand
optParseSet =
    [| MkSetCommand
       (liftAp targetPart)
       (liftAp setterPart)
    |]

defaultSet : Default SetCommand'
defaultSet = MkSetCommand
       (defaultPart targetPart)
       (defaultPart setterPart)

export
parseSet : List1 String -> ParseResult SetCommand
parseSet ("set":::xs) = do
    case parse (initBuilder $ defaultSet) optParseSet xs of
         InvalidMix reason => InvalidMix reason
         InvalidOption ys  => InvalidMix $ "Unknown option: " ++ ys.head
         Done builder      => maybe (InvalidMix "No test file given") Done $ build builder
parseSet xs = InvalidOption xs

export
helpSet : Help
helpSet = record {lastWords = Just footer} baseCommand
  where
    baseCommand : Help
    baseCommand = commandHelp {b = Builder SetCommand'}
      "set" "Set a global configuration for replica commands"
      (optParseSet)
      (Just "KEY=VALUE")
    footer : String
    footer =
      #"""
       Available keys, and description:
         replicaDir (or replica-dir, rDir) where replica stores internal information (default `./.replica`)
         goldenDir  (or golden-dir, gDir) where replica stores golden values (default `./.replica/tests`)
         colour     (or color) do we used colored output or not? (true or false, default `true`)
         ascii      do we keep away emojis or not? (true or false, default `false`)
         diff       command used to display diff
                    (known value: diff, git, native, other strings are considered as custom command)
                    (default: `native`)
         log        log level (default: `none`)
                    (known value: debug, info, warning, critical)
         testFile   (or jsonFile, test) the path of the test file to use (prefer a relative path)
                    (no default)
       """#

