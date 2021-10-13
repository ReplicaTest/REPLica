module Replica.Core.Types

import Data.String
import Data.List
import Data.List1
import Language.JSON

%default total

public export
data OrderSensitive = Ordered | Whatever

export
Show OrderSensitive where
  show Ordered = "Ordered"
  show Whatever = "Whatever"

public export
data Part = StdOut | StdErr | FileName String

export
Show Part where
  show StdOut = "StdOut"
  show StdErr = "StdErr"
  show (FileName x) = "(File \{x})"

public export
data Expectation
   = Exact String
   | StartsWith String
   | EndsWith String
   | Partial OrderSensitive (List String)
   | Generated

public export
ExpectationError : Expectation -> Type
ExpectationError Generated = Maybe String
ExpectationError (Partial Ordered xs) = String
ExpectationError (Partial Whatever xs) = List1 String
ExpectationError x = Unit


export
Show Expectation where
  show (Exact x) = "Exact \{show x}"
  show (StartsWith x) = "StartsWith x"
  show (EndsWith x) = "EndsWith \{show x}"
  show (Partial x xs) = "Partial \{show x} \{show xs}"
  show Generated = "Generated"

public export
record Test where
  constructor MkTest
  name: String
  pending : Bool
  description: Maybe String
  require : List String
  workingDir : Maybe String
  tags: List String
  suite : Maybe String
  beforeTest : List String
  afterTest : List String
  command: String
  input : Maybe String
  status : Maybe (Either Bool Nat)
  spaceSensitive : Bool
  stdOut : List Expectation
  stdErr : List Expectation
  files : List (String, List Expectation)

export
(.expectations) : Test -> List (Part, List Expectation)
(.expectations) t = (StdOut, t.stdOut) :: (StdErr, t.stdErr) :: (mapFst FileName <$> t.files)

export
Show Test where
  show x = unwords
    [ "MkTest"
    , show x.name
    , show x.pending
    , show x.description
    , show x.require
    , show x.workingDir
    , show x.tags
    , show x.beforeTest
    , show x.afterTest
    , show x.command
    , show x.input
    , show x.status
    , show x.spaceSensitive
    , show x.stdOut
    , show x.stdErr
    , show x.files
    ]

export
defaultExpectedOutput : String
defaultExpectedOutput = "expected"

export
defaultExpectedError : String
defaultExpectedError = "expected.err"

export
defaultFile : String
defaultFile = "file"

export
defaultError : String
defaultError = "error"

export
defaultOutput : String
defaultOutput = "output"

export
defaultInput : String
defaultInput = "input"

export
defaultStatus : String
defaultStatus = "status"



public export
record Replica where
  constructor MkReplica
  tests: List Test

public export
data FailReason : Type where
  WrongStatus : (status : Nat) -> (expected : Either Bool Nat) -> FailReason
  WrongOutput : Part -> String -> List1 (e : Expectation **  ExpectationError e) -> FailReason
  ExpectedFileNotFound : String -> FailReason

export
isNoGolden : FailReason -> Bool
isNoGolden (WrongOutput source given xs) = hasGolden $ forget xs
  where
    hasGolden : List (e : Expectation ** ExpectationError e) -> Bool
    hasGolden [] = False
    hasGolden ((Generated ** Nothing) :: _) = True
    hasGolden (_ :: xs) = hasGolden xs
isNoGolden _ = False

export
isMismatch : FailReason -> Bool
isMismatch (WrongOutput source given xs) = hasMismatch $ forget xs
  where
    hasMismatch : List (e : Expectation ** ExpectationError e) -> Bool
    hasMismatch [] = False
    hasMismatch ((Generated ** Nothing) :: xs) = hasMismatch xs
    hasMismatch (_ :: xs) = True
isMismatch _ = False

export
displaySource : Part -> String
displaySource StdOut = "standard output"
displaySource StdErr = "standard error"
displaySource (FileName x) = "file \{show x}"

export
Eq Part where
  (==) StdOut StdOut = True
  (==) StdErr StdErr = True
  (==) (FileName x) (FileName y) = x == y
  (==) _ _ = False

export
displayFailReason : FailReason -> List String
displayFailReason (WrongStatus x (Left True)) = pure "[Fails while it should pass : \{show x}]"
displayFailReason (WrongStatus _ (Left False)) = pure "[Pass but it should fail]"
displayFailReason (WrongStatus x (Right y)) = pure "[Status error: got \{show x}, expected \{show y}]"
displayFailReason (ExpectedFileNotFound src) = pure "[Missing expected file \"\{src}\"]"
displayFailReason w@(WrongOutput src given reasons) = join
  [ guard (isNoGolden w) $> "[Missing Golden for \{displaySource src}]"
  , guard (isNoGolden w) $> "[Unexpected content for \{displaySource src}]"
  ]

namespace FailReason

  encodePart : Part -> (String, JSON)
  encodePart StdOut = ("source", JString "stdout")
  encodePart StdErr = ("source", JString "stderr")
  encodePart (FileName x) = ("source", JObject [("file", JString x)])

  encodeFailure : (e : Expectation ** ExpectationError e) -> List (String, JSON)
  encodeFailure (MkDPair (Exact x) ()) = [("exact", JString x)]
  encodeFailure (MkDPair (StartsWith x) ()) = [("start", JString x)]
  encodeFailure (MkDPair (EndsWith x) ()) = [("end", JString x)]
  encodeFailure (MkDPair (Partial Ordered xs) snd) =
    [("consecutive", JArray $ JString <$> xs), ("notFound", JString snd)]
  encodeFailure (MkDPair (Partial Whatever xs) snd) =
    [("contains", JArray $ JString <$> xs), ("notFound", JArray $ forget $ JString <$> snd)]
  encodeFailure (MkDPair Generated Nothing) = [("generated", JNull)]
  encodeFailure (MkDPair Generated (Just x)) = [("generated", JString x)]

  export
  toJSON : FailReason -> JSON
  toJSON (WrongStatus x y) = JObject
    [("type", JString "status"), ("expected", either JBoolean (JNumber . cast) y), ("given", JNumber $ cast x)]
  toJSON (ExpectedFileNotFound src) = JObject
    [("type", JString "missing"), ("expected", JString src)]
  toJSON (WrongOutput src given err) = JObject $
    encodePart src :: ("type", JString "output") :: ("given", JString given) ::
    [("errors", JArray $ map (JObject . encodeFailure) $ forget err)]

public export
data TestResult
  = Success
  | Fail (List FailReason)
  | Skipped

namespace TestResult

  export
  toJSON : TestResult -> JSON
  toJSON Success = JString "Success"
  toJSON (Fail xs) = JObject [("Fail", JArray $ map toJSON xs)]
  toJSON Skipped = JString "Skipped"

  export
  isSuccess : TestResult -> Bool
  isSuccess Success = True
  isSuccess _ = False

public export
data TestError
  = FileSystemError String
  | InitializationFailed String
  | WrapUpFailed TestResult String
  | RequirementsFailed String
  | Inaccessible

namespace TestError

  export
  toJSON : TestError -> JSON
  toJSON (FileSystemError x) =
    JObject [("type", JString "FileSystemError") , ("content", JString x)]
  toJSON (InitializationFailed x) =
    JObject [("type", JString "InitializationFailed") , ("content", JString x)]
  toJSON (WrapUpFailed x y) =
    JObject [("type", JString "WrapUpFailed"),  ("result", toJSON x), ("content", JString y)]
  toJSON (RequirementsFailed x) =
    JObject [("type", JString "RequirementsFailed"), ("content", JString x)]
  toJSON Inaccessible =
    JObject [("type", JString "Inaccessible")]

export
displayTestError : TestError -> String
displayTestError (FileSystemError x) = "File error: \{x}"
displayTestError (InitializationFailed x) = "Before test action failed: \{x}"
displayTestError (WrapUpFailed x y) =  "After test action failed: \{y}"
displayTestError (RequirementsFailed x) = "Test rely on test \{x}, which failed"
displayTestError Inaccessible = "Test rely on other tests that weren't run"

export
isFullSuccess : Either TestError TestResult -> Bool
isFullSuccess (Right Success) = True
isFullSuccess _ = False

public export
record Stats where
  constructor MkStats
  successes : Nat
  failures  : Nat
  errors    : Nat
  skipped   : Nat

export
Semigroup Stats where
  (<+>) x y = MkStats
    (x.successes + y.successes)
    (x.failures + y.failures)
    (x.errors + y.errors)
    (x.skipped + y.skipped)

export
Monoid Stats where
  neutral = MkStats 0 0 0 0

export
asStats : List (Either TestError TestResult) -> Stats
asStats = foldMap go
  where
    go : Either TestError TestResult -> Stats
    go (Left x) = record {errors = 1} neutral
    go (Right Success) = record {successes = 1} neutral
    go (Right (Fail xs)) = record {failures = 1} neutral
    go (Right Skipped) = record {skipped = 1} neutral

export
countTests : Stats -> Nat
countTests x = x.successes + x.failures + x.errors + x.skipped

export
resultToJSON : Either TestError TestResult -> JSON
resultToJSON (Left x) = JObject [("Error", toJSON x)]
resultToJSON (Right x) = toJSON x

export
reportToJSON : List (String, Either TestError TestResult) -> JSON
reportToJSON = JObject . map (map resultToJSON)
