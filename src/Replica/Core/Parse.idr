module Replica.Core.Parse

import Replica.Core.Types
import Replica.Other.Validation

import Data.List
import Language.JSON

%default total

validateDesc : Maybe JSON -> Validation (List String) (Maybe String)
validateDesc Nothing = Valid empty
validateDesc (Just JNull) = Valid empty
validateDesc (Just (JString x)) = Valid $ Just $ x
validateDesc (Just x)
  = Error ["Test description should be a string, found: \{show x}"]

validateWD : Maybe JSON -> Validation (List String) (Maybe String)
validateWD Nothing = Valid empty
validateWD (Just JNull) = Valid empty
validateWD (Just (JString x)) = Valid $ Just $ x
validateWD (Just x)
  = Error ["Test working directory should be a string, found: \{show x}"]

validateTag : JSON -> Validation (List String) String
validateTag (JString x) = Valid x
validateTag x = Error ["A tag must be a string, found: \{show x}"]

validateTagList : Maybe JSON -> Validation (List String) (List String)
validateTagList Nothing = Valid empty
validateTagList (Just JNull) = Valid empty
validateTagList (Just (JString x)) = Valid $ pure x
validateTagList (Just (JArray ys)) = traverse validateTag ys
validateTagList (Just x) = Error
  ["Tags must contain a tag (string) or an array of tags, found \{show x}"]

validateCommandItem : JSON -> Validation (List String) String
validateCommandItem (JString x) = Valid x
validateCommandItem x = Error ["An item must be a string, found \{show x}"]

validateCommandList : (commandName : String) -> Maybe JSON -> Validation (List String) (List String)
validateCommandList _ Nothing = Valid empty
validateCommandList _ (Just JNull) = Valid empty
validateCommandList _ (Just (JString x)) = Valid $ pure x
validateCommandList _ (Just (JArray ys)) = traverse validateCommandItem ys
validateCommandList cmd (Just x) = Error
  ["\{cmd} must contain a command (string) or an array of commands, found \{show x}"]

validateBefore : Maybe JSON -> Validation (List String) (List String)
validateBefore = validateCommandList "beforeTest"

validateAfter : Maybe JSON -> Validation (List String) (List String)
validateAfter = validateCommandList "afterTest"

validateCommand : Maybe JSON -> Validation (List String) String
validateCommand (Just (JString str)) = Valid str
validateCommand Nothing = Error ["command is missing"]
validateCommand (Just x) = Error ["Command must be a string, found \{show x}"]

validateStatus : Maybe JSON -> Validation (List String) (Maybe Bool)
validateStatus Nothing = Valid empty
validateStatus (Just JNull) = Valid empty
validateStatus (Just (JBoolean x)) = Valid $ Just x
validateStatus (Just x) = Error ["Status should be a booleas, found: \{show x}"]

jsonToTest : String -> JSON -> Validation (List String) Test
jsonToTest str (JObject xs) =
  [| MkTest
  (pure str)
  (validateDesc $ lookup "description" xs)
  (validateWD $ lookup "workingDir" xs)
  (validateTagList $ lookup "tags" xs)
  (validateBefore $ lookup "beforeTest" xs)
  (validateAfter $ lookup "afterTest" xs)
  (validateCommand $ lookup "command" xs)
  (validateStatus $ lookup "succeed" xs)
  |]
jsonToTest str json =
  Error ["Expecting a JSON object for testa '\{str}' and got: \{show json}"]

export
jsonToReplica : JSON -> Validation (List String) Replica
jsonToReplica (JObject xs) = [| MkReplica $ traverse (uncurry jsonToTest) xs |]
jsonToReplica _ = Error ["Replica test file must be a JSON object"]
