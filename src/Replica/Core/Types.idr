module Replica.Core.Types

%default total

public export
record Test where
  constructor MkTest
  name: String
  description: Maybe String
  workingDir : Maybe String
  -- tags: List String
  -- require : List String
  beforeTest : List String
  afterTest : List String
  -- env: Map String String
  command: String
  mustSucceed : Maybe Bool

export
defaultExpected : Test -> String
defaultExpected t = "\{t.name}.expected"

export
defaultOutput : Test -> String
defaultOutput t = "\{t.name}.output"


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
Show FailReason where
  show (WrongStatus True) = "Fails while it should pass"
  show (WrongStatus expected) = "Pass but it should fail"
  show (WrongOutput x) = "WrongOutput"

public export
data TestResult
  = Success
  | Fail (List FailReason)

public export
data TestError
  = FileSystemError String
  | InitializationFailed String
  | WrapUpFailed TestResult String

export
Show TestError where
  show (FileSystemError x) = "File error: \{x}"
  show (InitializationFailed x) = "Before test action failed: \{x}"
  show (WrapUpFailed x y) =  "After test action failed: \{y}"

public export
record Stats where
  constructor MkStats
  successes : Nat
  failures  : Nat
  errors    : Nat

export
Semigroup Stats where
  (<+>) x y = MkStats
    (x.successes + y.successes)
    (x.failures + y.failures)
    (x.errors + y.errors)

export
Monoid Stats where
  neutral = MkStats 0 0 0

export
asStats : List (Either TestError TestResult) -> Stats
asStats = foldMap go
  where
    go : Either TestError TestResult -> Stats
    go (Left x) = record {errors = 1} neutral
    go (Right Success) = record {successes = 1} neutral
    go (Right (Fail xs)) = record {failures = 1} neutral

export
countTests : Stats -> Nat
countTests x = x.successes + x.failures + x.errors
