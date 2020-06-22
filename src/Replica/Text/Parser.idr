module Replica.Text.Parser

import public Text.Parser

import public Replica.Path

import System.File


import Replica.Text.Lexer

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

public export
Value : Type
Value = Either Path String


export
keyValue : Rule (String, Value)
keyValue =  MkPair <$> ident <*> (equals *> (map Left path <|> map Right string))

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

