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


suite : Rule $ Validation BuildError SuiteBuilder
suite = map foldBuilder (suitePreamble *> some keyValue)
  where
    addTests : Value -> ValidationM List BuildError (SuiteM List)
    addTests (SPath x) = pure $ record {tests = pure [x]} emptyBuilder
    addTests (LPath xs) = pure $ record {tests = pure (fst xs)} emptyBuilder
    addTests (SString x) = Error $ InvalidTestPath x
    addTests (LString xs) = Error $ InvalidTestPath $ unwords $ fst xs

    builderPart : String -> Value -> Validation BuildError SuiteBuilder
    builderPart "test" y = addTests y
    builderPart "tests" y = ?builderPart_rhs
    builderPart _ _ = pure emptyBuilder

    foldBuilder : List (String, Value) -> Validation BuildError SuiteBuilder
    foldBuilder = concatMap (uncurry builderPart)


covering export
parseSuite : (filename : String) -> IO (Either (ParsingError (List BuildError)) Suite)
parseSuite filename = do
  Right str <- readFile filename
    | Left err => pure $ Left $ FileNotFound err
  pure $ runParser str

  where

    mapError : (a -> b) -> Either a c -> Either b c
    mapError f (Left x) = Left (f x)
    mapError f (Right x) = Right x

    runParser : String -> Either (ParsingError (List BuildError)) Suite
    runParser  str = do
      tokens <- mapError LexerFailed $ lex str
      (cfg, []) <- mapError ParserFailed $ parse suite tokens
        | (_, err) => Left $ ParserFailed $ Error "Cannot parse tokens"  err
      mapError TargetError $ toEither $ map build cfg
