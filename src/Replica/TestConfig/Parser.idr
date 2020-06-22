module Replica.TestConfig.Parser

import public Text.Parser

import System.File


import Replica.TestConfig.Core
import Replica.TestConfig.Lexer

public export
Rule : Type -> Type
Rule = Grammar (TokenData Token) True

export
key : String -> Rule ()
key k = terminal ("Expected key " ++ k)
  \x => case tok x of
             Word y      => guard $ k == y
             StringLit y => guard $ k == y
             _ => Nothing

export
ident : Rule String
ident = terminal "Expected ident"
  \x => case tok x of
             Word y      => Just y
             _ => Nothing

export
equals : Rule ()
equals = terminal "Expected equals"
  \x => case tok x of
             Equals => Just ()
             _ => Nothing

export
stringLit : Rule String
stringLit = terminal "Expected String literal"
  \x => case tok x of
             StringLit str => Just str
             _ => Nothing

export
string : Rule String
string = terminal "Expected string"
  \x => case tok x of
             Word y      => Just y
             StringLit y => Just y
             _ => Nothing

export
separator : Rule ()
separator = terminal "Expected String literal"
  \x => case tok x of
             Separator str => Just ()
             _ => Nothing

export
identPath : Rule Path
identPath = sepBy1' separator ident

export
path : Rule Path
path = identPath <|> fail "A Path is expected"

export
Value : Type
Value = Either Path String


export
test : Rule Path
test = (key "test" *> path) <|>fail "No test preamble"

export
keyValue : Rule (String, Value)
keyValue =  MkPair <$> ident <*> (equals *> (map Left path <|> map Right string))

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


public export
data ParsingError err
  = FileNotFound FileError
  | LexerFailed (Int, Int, String)
  | ParserFailed (ParseError (TokenData Token))
  | TargetError err

export
displayParsingError : (err -> String) -> ParsingError err -> String
displayParsingError _ (FileNotFound x)
  = show x
displayParsingError _ (LexerFailed (l,c,msg))
  = "Syntax error: line " ++ show l ++ "; column: " ++ show c ++"; " ++ msg
displayParsingError _ (ParserFailed (Error msg xs))
  = "Cannot parse file: " ++ msg
displayParsingError f (TargetError x)
  = "Cannot build expected value: " ++ f x

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
        | (_, err) => Left $ ParserFailed (Error "Cannot parse tokens"  err)
      mapError TargetError $ build $ foldBuilder cfg
