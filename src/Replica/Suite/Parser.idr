module Replica.Suite.Parser

import Data.Strings
import System.File

import public Replica.Suite.Core
import public Replica.Text.Parser
import public Replica.Validation

%default total

export
suitePreamble : Rule ()
suitePreamble = key "suite"


foldBuilder : List (String, Value) -> Validation BuildError SuiteBuilder
foldBuilder = concatMap (uncurry builderPart)

  where
    addTests : Value -> ValidationM List BuildError (SuiteM List)
    addTests (SPath x) = pure $ record {tests = pure [x]} emptyBuilder
    addTests (LPath xs) = pure $ record {tests = pure (fst xs)} emptyBuilder
    addTests (SString x) = Error $ InvalidTestPath x
    addTests (LString xs) = Error $ InvalidTestPath $ unwords $ fst xs

    builderPart : String -> Value -> Validation BuildError SuiteBuilder
    builderPart "test" y = addTests y
    builderPart "tests" y = addTests y
    builderPart _ _ = pure emptyBuilder

export
toSuite : List (String, Value) -> Validation BuildError Suite
toSuite xs = case foldBuilder xs of
  ErrorM x => ErrorM x
  Valid x  => pure $ build x

params : Rule $ List (String, Value)
params = suitePreamble *> some keyValue

export
suite : Rule $ Validation BuildError Suite
suite = map toSuite params

covering export
parseSuite : (filename : String) -> IO (Either (ParsingError (List BuildError)) Suite)
parseSuite = parseWith params toSuite
