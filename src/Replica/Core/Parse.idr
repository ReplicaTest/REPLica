module Replica.Core.Parse

import Replica.Core.Types
import Replica.Other.Validation

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Language.JSON

%default total

allString : List JSON -> Validation (List String) (List String)
allString =  traverse \j => case j of
  JString str => Valid str
  _ => Error ["Invalid expectation part: \{show j}"]

validatePending : Maybe JSON -> Validation (List String) Bool
validatePending Nothing = Valid False
validatePending (Just (JBoolean x)) = Valid x
validatePending x = Error ["Pending must be a boolean, found: \{show x}"]

validateDesc : Maybe JSON -> Validation (List String) (Maybe String)
validateDesc Nothing = Valid empty
validateDesc (Just JNull) = Valid empty
validateDesc (Just (JString x)) = Valid $ Just $ x
validateDesc (Just x)
  = Error ["Test description should be a string, found: \{show x}"]

validateRequire : JSON -> Validation (List String) String
validateRequire (JString x) = Valid x
validateRequire x = Error ["A requirement must be a testname, found: \{show x}"]

validatOrderSensitive : Maybe JSON -> Validation (List String) OrderSensitive
validatOrderSensitive Nothing = Valid Ordered
validatOrderSensitive (Just (JBoolean x)) = Valid $ if x then Ordered else Whatever
validatOrderSensitive (Just x) = Error
  ["OrderSensitive must be a boolean, found \{show x}"]

validateExpectation : String -> JSON -> Validation (List String) (Maybe Expectation)
validateExpectation "generated" (JBoolean True) = Valid $ Just $ Generated
validateExpectation "generated" (JBoolean False) = Valid Nothing
validateExpectation "exact" (JString str) = Valid $ Just $ Exact str
validateExpectation "exact" json = Error ["exact expectation requires a string, found: \{show json}"]
validateExpectation "start" (JString str) = Valid $ Just $ StartsWith str
validateExpectation "start" json = Error ["start expectation requires a string, found: \{show json}"]
validateExpectation "end" (JString str) = Valid $ Just $ EndsWith str
validateExpectation "end" json = Error ["end expectation requires a string, found: \{show json}"]
validateExpectation "consecutive" (JArray []) = Valid Nothing
validateExpectation "consecutive" (JArray xs) = Just . Partial Ordered <$> allString xs
validateExpectation "consecutive" json =
  Error ["consecutive expectation requires a string, found: \{show json}"]
validateExpectation "contains" (JArray []) = Valid Nothing
validateExpectation "contains" (JArray xs) = Just . Partial Whatever <$> allString xs
validateExpectation "contains" json =
  Error ["consecutive expectation requires a string, found: \{show json}"]
validateExpectation x json = Error ["Unknown expectation: \{show x}"]

defaultExpectation : List (Part, List Expectation)
defaultExpectation = [(StdOut, [Generated])]

validateExpectations : Maybe JSON -> Validation (List String) (List (Part, List Expectation))
validateExpectations (Just (JString x)) = Valid [(StdOut, [Exact x])]
validateExpectations (Just (JArray [])) = Valid defaultExpectation
validateExpectations (Just (JArray xs)) = pure . MkPair StdOut . pure . Partial Whatever <$> allString xs
validateExpectations (Just (JObject [])) = Valid defaultExpectation
validateExpectations (Just (JObject o)) = traverse go o
  where
    readStdout : String -> Maybe Part
    readStdout x = guard (toLower x == "stdout") $> StdOut
    readStderr : String -> Maybe Part
    readStderr x = guard (toLower x == "stderr") $> StdErr
    readPart : String -> Part
    readPart x = fromMaybe (FileName x) (readStdout x <|> readStderr x)
    go : (String, JSON) -> Validation (List String) (Part, List Expectation)
    go (x, JNull) = Valid $ MkPair (readPart x) [Generated]
    go (x, JBoolean True) = Valid $ MkPair (readPart x) [Generated]
    go (x, JBoolean False) = Valid $ MkPair (readPart x) []
    go (x, JArray []) = Valid $ MkPair (readPart x) [Generated]
    go (x, JArray y) = MkPair (readPart x) . pure . Partial Ordered <$> allString y
    go (x, JString y) = Valid $ MkPair (readPart x) [Exact y]
    go (x, JObject []) = Valid $ MkPair (readPart x) [Generated]
    go (x, JObject o) = MkPair (readPart x) . catMaybes <$> traverse (uncurry validateExpectation) o
    go (x, y) = Error ["Invalid expectation entry : \{show y}"]
validateExpectations (Just JNull) = Valid []
validateExpectations (Just (JBoolean x)) = Error ["Expectations can't be a boolean"]
validateExpectations (Just (JNumber x)) = Error ["Expectations can't be a number"]
validateExpectations Nothing = Valid defaultExpectation

validateRequireList : Maybe JSON -> Validation (List String) (List String)
validateRequireList Nothing = Valid empty
validateRequireList (Just JNull) = Valid empty
validateRequireList (Just (JString x)) = Valid $ pure x
validateRequireList (Just (JArray ys)) = traverse validateRequire ys
validateRequireList (Just x) = Error
  ["Require must contain a test name or an array of test names, found \{show x}"]

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

validateInput : JSON -> Validation (List String) String
validateInput (JString str) = Valid str
validateInput x = Error ["input must be a string, found \{show x}"]

validateStatus : Maybe JSON -> Validation (List String) (Maybe Bool)
validateStatus Nothing = Valid empty
validateStatus (Just JNull) = Valid empty
validateStatus (Just (JBoolean x)) = Valid $ Just x
validateStatus (Just x) = Error ["Status should be a boolean, found: \{show x}"]

validateSpaceSensitive : Maybe JSON -> Validation (List String) Bool
validateSpaceSensitive Nothing = Valid True
validateSpaceSensitive (Just (JBoolean x)) = Valid x
validateSpaceSensitive x = Error ["Pending must be a boolean, found: \{show x}"]

isString : JSON -> Validation (List String) String
isString (JString str) = Valid str
isString x = Error ["oututFiles content should be a list of string, found: \{show x}"]

validateFile : Maybe JSON -> Validation (List String) (Maybe String)
validateFile Nothing = Valid empty
validateFile (Just json) = Just <$> isString json

jsonToTest : String -> JSON -> Validation (List String) Test
jsonToTest str (JObject xs) =
  [| MkTest
  (pure str)
  (validatePending $ lookup "pending" xs)
  (validateDesc $ lookup "description" xs)
  (validateRequireList $ lookup "require" xs)
  (validateWD $ lookup "workingDir" xs)
  (validateTagList $ lookup "tags" xs)
  (validateBefore $ lookup "beforeTest" xs)
  (validateAfter $ lookup "afterTest" xs)
  (validateCommand $ lookup "command" xs)
  (traverse validateInput $ lookup "input" xs)
  (validateStatus $ lookup "succeed" xs)
  (validateSpaceSensitive $ lookup "spaceSensitive" xs)
  (validateExpectations $ lookup "expectation" xs <|> lookup "expectations" xs)
  (validateFile $ lookup "outputFile" xs)
  |]
jsonToTest str json =
  Error ["Expecting a JSON object for test '\{str}' and got: \{show json}"]

export
jsonToReplica : JSON -> Validation (List String) Replica
jsonToReplica (JObject xs) = [| MkReplica $ traverse (uncurry jsonToTest) xs |]
jsonToReplica _ = Error ["Replica test file must be a JSON object"]


parseWrongStatus : List (String, JSON) -> Maybe FailReason
parseWrongStatus xs = do
  JBoolean exp <- lookup "expected" xs
    | _ => Nothing
  pure $ WrongStatus exp

parseExpectedNotFound : List (String, JSON) -> Maybe FailReason
parseExpectedNotFound xs = do
  JString exp <- lookup "expected" xs
    | _ => Nothing
  pure $ ExpectedFileNotFound exp

parseWrongOutput : JSON -> Maybe (e : Expectation ** ExpectationError e)
parseWrongOutput (JObject [("exact", JString x)]) = Just (Exact x ** ())
parseWrongOutput (JObject [("start", JString x)]) = Just (StartsWith x ** ())
parseWrongOutput (JObject [("end", JString x)]) = Just (EndsWith x ** ())
parseWrongOutput (JObject [("generated", JString x)]) = Just (Generated ** Just x)
parseWrongOutput (JObject [("generated", JNull)]) = Just (Generated ** Nothing)
parseWrongOutput (JObject xs) = parseConsecutive xs <|> parseContains xs
  where
    parseConsecutive : List (String, JSON) -> Maybe (e : Expectation ** ExpectationError e)
    parseConsecutive xs = do
      JArray ys <- lookup "consecutive" xs
        | _ => Nothing
      let Valid x = allString ys
        | _ => Nothing
      JString notFound <- lookup "notFound" xs
        | _ => Nothing
      pure $ (Partial Ordered x ** notFound)
    parseContains : List (String, JSON) -> Maybe (e : Expectation ** ExpectationError e)
    parseContains xs = do
      JArray ys <- lookup "contains" xs
        | _ => Nothing
      let Valid x = allString ys
        | _ => Nothing
      JArray notFound <- lookup "notFound" xs
        | _ => Nothing
      let Valid (n::nf) = allString notFound
        | _ => Nothing
      pure $ (Partial Whatever x ** (n:::nf))
parseWrongOutput json = Nothing

parsePart : JSON -> Validation (List String) Part
parsePart (JString "stdout") = Valid StdOut
parsePart (JString "stderr") = Valid StdErr
parsePart (JObject [("file", JString x)]) = Valid $ FileName x
parsePart xs = Error ["Unable to parse a part, found: \{show xs}"]

parseFailReason : JSON -> Validation (List String) FailReason
parseFailReason (JObject xs) = do
  let Valid src = maybe (Error ["Missing source cause"]) parsePart $ lookup "source" xs
    | Error e => Error e
  case lookup "type" xs of
    Just (JString "output") => maybe
      (Error ["Invalid Wrong output content"])
      Valid $ do
        JString given <- lookup "given" xs
          | _ => Nothing
        JArray errs <- lookup "errors" xs
          | _ => Nothing
        (WrongOutput src given <$> (traverse parseWrongOutput errs >>= fromList))
    Just (JString "status") => maybe
      (Error ["Invalid Wrong output content"])
      Valid
      (parseWrongStatus xs)
    Just (JString "missing") => maybe
      (Error ["Invalid Wrong expected file not found content"])
      Valid
      (parseExpectedNotFound xs)
    pat => Error ["Invalid object content for a fail reason: \{show xs}"]
parseFailReason json =
  Error ["Expecting a JSON object for a fail reason, got: \{show json}"]

parseTestResult : JSON -> Validation (List String) TestResult
parseTestResult (JObject [("Fail", JArray cause)]) = map Fail $ traverse parseFailReason cause
parseTestResult (JString "Success") = Valid Success
parseTestResult (JString "Scekipped") = Valid Skipped
parseTestResult x = Error ["\{show x} can't be a valid result"]

parseTestError : JSON -> Validation (List String) TestError
parseTestError (JObject json) = case lookup "type" json of
  Just (JString "FileSystemError") => map
    FileSystemError
    $ case lookup "content" json of
           Just (JString str) => Valid str
           _ => Error ["No content found for a FileSystemError"]
  Just (JString "InitializationFailed") => map
    InitializationFailed
    $ case lookup "content" json of
           Just (JString str) => Valid str
           _ => Error ["No content found for a InitializationFailed"]
  Just (JString "RequirementsFailed") => map
    RequirementsFailed
    $ case lookup "content" json of
           Just (JString str) => Valid str
           _ => Error ["No content found for a RequirementsFailed"]
  Just (JString "WrapUpFailed") => map
    (uncurry WrapUpFailed)
    $ case (lookup "result" json, lookup "content" json) of
           (Just result, Just (JString content)) => map (flip MkPair content) $ parseTestResult result
           _ => Error ["No result and/or content found for a WrapUpFailed"]
  pat => Error ["Can't pares TestError content"]
parseTestError json =
  Error ["Expecting a JSON object for a test error and got: \{show json}"]


parseResult : JSON -> Validation (List String) (Either TestError TestResult)
parseResult (JObject [("Error", cause)]) = map Left $ parseTestError cause
parseResult (JObject [("Fail", JArray cause)]) = map (Right . Fail) $ traverse parseFailReason cause
parseResult (JString "Success") = Valid $ Right Success
parseResult x = Error ["\{show x} can't be a valid result"]

export
parseReport : JSON -> Validation (List String) (List (String, Either TestError TestResult))
parseReport (JObject xs) = traverse (\(str, res) => map (MkPair str) (parseResult res)) xs
parseReport x = Error ["A report must be an object. Found : \{show x}"]
