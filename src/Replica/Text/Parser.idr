module Replica.Text.Parser

import System.File


import public Text.Parser

import public Replica.Path
import public Replica.Text.Lexer
import public Replica.Validation


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
listSep : Rule ()
listSep = terminal "Expected listSep"
  \x => case tok x of
             ListSep => Just ()
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
listOf : Rule a -> Rule (DPair (List a) NonEmpty)
listOf r = (\x, xs => (x :: fst xs ** IsNonEmpty)) <$> r <*> (listSep *> sepBy1' listSep r)

export
path : Rule Path
path = identPath <|> fail "A Path is expected"

public export
data Value
  = SPath Path
  | SString String
  | LPath (DPair (List Path) NonEmpty)
  | LString (DPair (List String) NonEmpty)

value : Rule Value
value = choice $ the (List _)
  [ map LPath (listOf path)
  , map LString (listOf string)
  , map SPath path
  , map SString string
  ]

export
keyValue : Rule (String, Value)
keyValue =  MkPair <$> ident <*> (equals *> value)

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
mapError : (a -> b) -> Either a c -> Either b c
mapError f (Left x) = Left (f x)
mapError f (Right x) = Right x

export
entries : Rule (List (String, Value))
entries = some keyValue

covering export
parseWith : Rule (List (String, Value)) ->
             (List (String, Value) -> Validation err target) ->
             (filename : String) ->
             IO (Either (ParsingError (List err)) target)
parseWith params build filename = do
  Right str <- readFile filename
    | Left err => pure $ Left $ FileNotFound err
  pure $ runParser str

  where

    runParser : String -> Either (ParsingError (List err)) target
    runParser  str = do
      tokens <- mapError LexerFailed $ lex str
      (cfg, []) <- mapError ParserFailed $ parse params tokens
        | (_, err) => Left $ ParserFailed $ Error "Cannot parse tokens"  err
      mapError TargetError $ toEither $ build cfg
