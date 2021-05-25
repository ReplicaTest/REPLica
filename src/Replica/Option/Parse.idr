module Replica.Option.Parse

import Replica.Other.Validation
import Replica.Other.Decorated
import Replica.Option.Global

import Control.Monad.Identity
import Control.Monad.RWS
import Data.Either
import Data.List
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



jsonConfig : JSON -> Default Global'
jsonConfig (JObject xs) = MkGlobal
  ((lookup "replica-dir" xs <|> lookup "replicaDir" xs) >>= toMaybe . validateReplicaDir)
  ((lookup "golden-dir" xs <|> lookup "goldenDir" xs) >>= map Just . toMaybe . validateGoldenDir)
  ((lookup "colour" xs <|> lookup "color" xs) >>= toMaybe . validateColour)
  ((lookup "ascii" xs) >>= toMaybe . validateAscii)
  ((lookup "log-level" xs <|> lookup "logLevel" xs) >>= toMaybe . validateLogLevel)
  ((lookup "diff" xs) >>= toMaybe . validateDiff)
  ((lookup "testFile" xs) >>= toMaybe . validateFile)
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
