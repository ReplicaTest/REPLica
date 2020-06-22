module Replica.TestConfig.Parser

import public Text.Parser

import System.File


import Replica.TestConfig.Core
import Replica.Text.Lexer
import public Replica.Text.Parser

export
test : Rule Path
test = (key "test" *> path) <|>fail "No test preamble"

export
testConfig : Rule $ DPair (List (String, Value)) NonEmpty
testConfig = do
  x <- map (MkPair "test" . Left) test
  xs <- many keyValue
  pure ((x :: xs) ** IsNonEmpty)

foldBuilder : DPair (List (String, Value)) NonEmpty -> BuildTestConfig
foldBuilder = concatMap (uncurry toBuilder) . fst
  where
    pathToString : Path -> String
    pathToString (MkDPair path snd) = foldr1 (\x, y => x ++ "/" ++ y) path

    valueToString : Value -> String
    valueToString = either pathToString id

    toBuilder : String -> Value -> BuildTestConfig
    toBuilder "test" (Left y) = record {path = [y]} emptyBuilder
    toBuilder "exec" y = record {exec = [valueToString y]} emptyBuilder
    toBuilder "params" y = record {params = [valueToString y]} emptyBuilder
    toBuilder "input" y = record {inputFile = [Just $ valueToString y]} emptyBuilder
    toBuilder "inputFile" y = record {inputFile = [Just $ valueToString y]} emptyBuilder
    toBuilder "output" y = record {outputFile = [valueToString y]} emptyBuilder
    toBuilder "outputFile" y = record {outputFile = [valueToString y]} emptyBuilder
    toBuilder x y = emptyBuilder


export
parseTestConfig : (filename : String) -> IO (Either (ParsingError BuildError) TestConfig)
parseTestConfig filename = do
  Right str <- readFile filename
    | Left err => pure $ Left $ FileNotFound err
  pure $ runParser str

  where

    mapError : (a -> b) -> Either a c -> Either b c
    mapError f (Left x) = Left (f x)
    mapError f (Right x) = Right x

    runParser : String -> Either (ParsingError BuildError) TestConfig
    runParser  str = do
      tokens <- mapError LexerFailed $ lex str
      (cfg, []) <- mapError ParserFailed $ parse testConfig tokens
        | (_, err) => Left $ ParserFailed $ Error "Cannot parse tokens"  err
      mapError TargetError $ build $ foldBuilder cfg
