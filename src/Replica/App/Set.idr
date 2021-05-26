module Replica.App.Set

import Control.App
import Control.App.Console

import Data.List

import Language.JSON

import Replica.App.FileSystem
import Replica.App.Format
import Replica.App.Log
import Replica.App.Replica
import Replica.Command.Set
import Replica.Core

import Replica.Option.Global
import Replica.Other.Decorated
import Replica.Other.String
import Replica.Other.Validation

import System.Path

%default total

export
data HomeDir : Type where

export
data SetContext : Type where

export
updateConfig : List (String, JSON) -> Setter -> JSON
updateConfig = updateConfig' []
  where
    updateConfig' : List (String, JSON) -> List (String, JSON) -> Setter -> JSON
    updateConfig' xs [] (MkSetter key value) = JObject $ reverse $ (key, value) :: xs
    updateConfig' xs (x :: ys) s@(MkSetter key value) = if fst x == key
      then JObject $ reverse xs ++ ((key, value) :: ys)
      else updateConfig' (x :: xs) ys s



export
setReplica :
  FileSystem (FSError :: e) =>
  Has
    [ Console
    , State HomeDir (Maybe String)
    , State SetContext SetCommand
    , State GlobalConfig Global
    , Exception ReplicaError
    ] e => App e ()
setReplica = do
  setCtx <- get SetContext
  debug "Set: \{show setCtx}"
  tgt <- case setCtx.target of
              Local => pure $ "."</> ".replica.json"
              Global => do
                Just hd <- get HomeDir
                  | Nothing => throw $ InvalidJSON ["Can't access global config: No HOME"]
                pure $ hd </> ".replica.json"
  f <- catchNew (readFile tgt)
    (\err : FSError => case err of
                            (MissingFile x) => pure $ show $ JObject []
                            pat => throw $ InvalidJSON ["Error when accessing config"])
  let Just (JObject xs) = JSON.parse f
      | Nothing => throw $ InvalidJSON
         ["Can't parse current config \{show tgt} (either delete the file or fix its content)"]
      | Just _ => throw $ InvalidJSON
         ["Current config \{show tgt} is invalid (object expected) (either delete the file or fix its content)"]
  let newConfig = updateConfig xs !(setter <$> get SetContext)
  catchNew (writeFile tgt $ show newConfig)
    (\err : FSError => throw $ InvalidJSON ["Can't write config"])
