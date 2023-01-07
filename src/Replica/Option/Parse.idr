||| JSON parser for tests
module Replica.Option.Parse

import Replica.Other.Validation
import Replica.Other.Decorated
import Replica.Option.Global

import Control.Monad.Identity
import Control.Monad.RWS
import Data.Either
import Data.List
import Data.List1
import Data.String
import System
import System.File
import System.Path


import Language.JSON

validateReplicaDir : JSON -> Validation (List String) String
validateReplicaDir (JString x) = Valid x
validateReplicaDir x = Error ["Replica dir must be a string, found: \{show x}"]

validateGoldenDir : JSON -> Validation (List String) String
validateGoldenDir (JString x) = Valid x
validateGoldenDir x = Error ["Goldendir must be a string, found: \{show x}"]

validateColour : JSON -> Validation (List String) Bool
validateColour (JBoolean x) = Valid x
validateColour x = Error ["Colour must be a boolean, found: \{show x}"]

validateAscii : JSON -> Validation (List String) Bool
validateAscii (JBoolean x) = Valid x
validateAscii x = Error ["Ascii must be a boolean, found: \{show x}"]

invalidLogLevel : Validation (List String) a
invalidLogLevel = Error ["LogLevel must be either none, debug, info, warning, or critical"]

validateLogLevel : JSON -> Validation (List String) (Maybe LogLevel)
validateLogLevel (JString x) = maybe invalidLogLevel Valid $ readLogLevel x
validateLogLevel x = invalidLogLevel

validateDiff : JSON -> Validation (List String) DiffCommand
validateDiff (JString x) = Valid $ readDiffCommand x
validateDiff x = Error ["Diff should be a string"]

validateFile : JSON -> Validation (List String) String
validateFile (JString x) = Valid x
validateFile x = Error ["File should be a string"]

export
record ConfigValue where
  constructor CfgValue
  primary : String
  secondaries : List String
  parser : String -> Maybe JSON

(.keys) : ConfigValue -> List1 String
(.keys) x = x.primary ::: x.secondaries

export
checkKey : ConfigValue -> String -> Maybe String
checkKey x y = guard (y `elem` forget x.keys) $> x.primary

export
getKey : ConfigValue -> (String, a) -> Maybe a
getKey x (k, v) = guard (k `elem` forget x.keys) $> v

export
jsonFor: ConfigValue -> (String, String) -> Maybe (String, JSON)
jsonFor x kv = getKey x kv >>= map (MkPair x.primary) . x.parser

replicaDirValues : ConfigValue
replicaDirValues = CfgValue "replicaDir" ["replica-dir", "rDir"]
  (Just . JString)

goldenDirValues : ConfigValue
goldenDirValues = CfgValue "goldenDir" ["golden-dir", "gDir"]
  (Just . JString)

colourValues : ConfigValue
colourValues = CfgValue "colour" ["color"]
  (\x => case toLower x of
              "true" => Just $ JBoolean True
              "false" => Just $ JBoolean False
              _ => Nothing)

asciiValues : ConfigValue
asciiValues = CfgValue "ascii" []
  (\x => case toLower x of
              "true" => Just $ JBoolean True
              "false" => Just $ JBoolean False
              _ => Nothing)

logValues : ConfigValue
logValues = CfgValue "logLevel" ["log-level", "log"]
  (\x => readLogLevel x $> JString x)

diffValues : ConfigValue
diffValues = CfgValue "diff" []
  (Just . JString)

testFileValues : ConfigValue
testFileValues= CfgValue "testFile" ["jsonFile", "test"]
  (Just . JString)

export
configValues : List ConfigValue
configValues =
  [ replicaDirValues
  , goldenDirValues
  , colourValues
  , asciiValues
  , logValues
  , diffValues
  , testFileValues]

jsonConfig : JSON -> Default Global'
jsonConfig (JObject xs) = MkGlobal
  (foldMap (getKey replicaDirValues) xs >>= toMaybe . validateReplicaDir)
  (foldMap (getKey goldenDirValues) xs >>= map Just . toMaybe . validateGoldenDir)
  (foldMap (getKey colourValues) xs >>= toMaybe . validateColour)
  (foldMap (getKey asciiValues) xs >>= toMaybe . validateAscii)
  (foldMap (getKey logValues) xs >>= toMaybe . validateLogLevel)
  (foldMap (getKey diffValues) xs >>= toMaybe . validateDiff)
  (foldMap (getKey testFileValues) xs >>= toMaybe . map pure . validateFile)
jsonConfig json = neutral

export
configPath : IO (List String)
configPath = do
  Just global <- map (</> ".replica.json") <$> getEnv "HOME"
    | Nothing => pure [".replica.json"]
  pure [global </> ".replica.json", ".replica.json"]

export
givenConfig : IO (Default Global')
givenConfig = do
  path <- configPath
  xs <- catMaybes . map JSON.parse . rights <$> traverse readFile path
  pure $ concatMap jsonConfig xs
