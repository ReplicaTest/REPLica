module Replica.TestConfig.Parser

import Data.Strings
import System.File

import Replica.TestConfig.Core
import Replica.Text.Lexer


import public Text.Parser

import public Replica.Text.Parser
import public Replica.Validation

%default total

export
testPreamble : Rule Path
testPreamble = (key "test" *> path) <|>fail "No test preamble"

params : Rule $ List (String, Value)
params = do
  x <- map (MkPair "test" . SPath) testPreamble
  map (x ::) entries

foldBuilder : List (String, Value) -> Validation BuildError BuildTestConfig
foldBuilder = concatMap (uncurry toBuilder)
  where
    pathToString : Path -> String
    pathToString (MkDPair path snd) = foldr1 (\x, y => x ++ "/" ++ y) path

    valueToStrings : Value -> List String
    valueToStrings (SPath x) = pure $ pathToString x
    valueToStrings (SString x) = pure x
    valueToStrings (LPath xs) = map pathToString $ fst xs
    valueToStrings (LString xs) = fst xs

    toBuilder : String -> Value -> Validation BuildError BuildTestConfig
    toBuilder "test" (SPath y) = pure $ record {path = [y]} emptyBuilder
    toBuilder "test" (LPath y) = pure $ record {path = fst y} emptyBuilder
    toBuilder "test" y = Error $ InvalidPath $ unwords $ valueToStrings y
    toBuilder "exec" y = pure $ record {exec = valueToStrings y} emptyBuilder
    toBuilder "params" y = pure $ record {params = valueToStrings y} emptyBuilder
    toBuilder "input" y = pure $ record {inputFile = map Just $ valueToStrings y} emptyBuilder
    toBuilder "inputFile" y = pure $ record {inputFile = map Just $ valueToStrings y} emptyBuilder
    toBuilder "output" y = pure $ record {outputFile = valueToStrings y} emptyBuilder
    toBuilder "outputFile" y = pure $ record {outputFile = valueToStrings y} emptyBuilder
    toBuilder x y = pure $ emptyBuilder

export
toTestConfig : List (String, Value) -> Validation BuildError TestConfig
toTestConfig xs = case foldBuilder xs of
  ErrorM x => ErrorM x
  Valid x  => build x

export
testConfig : Rule $ Validation BuildError TestConfig
testConfig = map toTestConfig params

covering export
parseTestConfig : (filename : String) -> IO (Either (ParsingError (List BuildError)) TestConfig)
parseTestConfig = parseWith params toTestConfig
