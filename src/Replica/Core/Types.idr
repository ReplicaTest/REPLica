module Replica.Core.Types

import Data.String
import Language.JSON

%default total

public export
record Test where
  constructor MkTest
  name: String
  pending : Bool
  description: Maybe String
  require : List String
  workingDir : Maybe String
  tags: List String
  beforeTest : List String
  afterTest : List String
  -- env: Map String String
  command: String
  input : Maybe String
  mustSucceed : Maybe Bool

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
    , show x.mustSucceed
    ]

export
defaultExpected : String
defaultExpected = "expected"

export
defaultOutput : String
defaultOutput = "output"

export
defaultInput : String
defaultInput = "input"



public export
record Replica where
  constructor MkReplica
  tests: List Test

public export
data OutputError
  = GoldenIsMissing
  | DifferentOutput String String

public export
data FailReason : Type where
  WrongStatus : (expectSuccess : Bool) -> FailReason
  WrongOutput : OutputError -> FailReason

export
displayFailReason : FailReason -> String
displayFailReason (WrongStatus True) = "[Fails while it should pass]"
displayFailReason (WrongStatus False) = "[Pass but it should fail]"
displayFailReason (WrongOutput GoldenIsMissing) = "[Missing Golden]"
displayFailReason (WrongOutput x) = "[WrongOutput]"

namespace FailReason

  export
  toJSON : FailReason -> JSON
  toJSON (WrongStatus e) = JObject
    [("type", JString "status"), ("expected", JBoolean e), ("given", JBoolean $ not e)]
  toJSON (WrongOutput GoldenIsMissing) = JObject
    [("type", JString "output"), ("reason", JString "Missing")]
  toJSON (WrongOutput (DifferentOutput x y)) = JObject
    [("type", JString "output"), ("expected", JString x), ("given", JString y)]

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
isSuccess : Either TestError TestResult -> Bool
isSuccess (Right Success) = True
isSuccess _ = False

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
