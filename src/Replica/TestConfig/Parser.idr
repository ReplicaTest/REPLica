module Replica.TestConfig.Parser

import Data.Strings

import public Text.Parser

import System.File


import Replica.TestConfig.Core
import Replica.Text.Lexer
import public Replica.Text.Parser
import public Replica.Validation

export
test : Rule Path
test = (key "test" *> path) <|>fail "No test preamble"

export
testConfig : Rule $ DPair (List (String, Value)) NonEmpty
testConfig = do
  x <- map (MkPair "test" . SPath) test
  xs <- many keyValue
  pure ((x :: xs) ** IsNonEmpty)

foldBuilder : DPair (List (String, Value)) NonEmpty -> Validation BuildError BuildTestConfig
foldBuilder = concatMap (uncurry toBuilder) . fst
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
parseTestConfig : (filename : String) -> IO (Either (ParsingError (List BuildError)) TestConfig)
parseTestConfig filename = do
  Right str <- readFile filename
    | Left err => pure $ Left $ FileNotFound err
  pure $ runParser str

  where

    mapError : (a -> b) -> Either a c -> Either b c
    mapError f (Left x) = Left (f x)
    mapError f (Right x) = Right x

    runParser : String -> Either (ParsingError (List BuildError)) TestConfig
    runParser  str = do
      tokens <- mapError LexerFailed $ lex str
      (cfg, []) <- mapError ParserFailed $ parse testConfig tokens
        | (_, err) => Left $ ParserFailed $ Error "Cannot parse tokens"  err
      builder <- mapError TargetError $ toEither $ foldBuilder cfg
      mapError TargetError $ toEither $ build builder
