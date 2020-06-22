module Replica.TestConfig.Lexer

import public Text.Lexer
import public Text.Parser

import Data.List
import Data.Strings

%default total

public export
data Token
  = Equals
  | Word String
  | Comment String
  | Space
  | Separator String
  | Path String
  | StringLit String

%default total

lineComment : Lexer
lineComment = Text.Lexer.lineComment (exact "--")

%hide Text.Lexer.lineComment

blockComment : Lexer
blockComment = Text.Lexer.blockComment (exact "{-") (exact "-}")

%hide Text.Lexer.blockComment

spacesOrNewlines : Lexer
spacesOrNewlines = some (space <|> newline)

comment : Lexer
comment = lineComment <|> blockComment

equals : Lexer
equals = is '='

letterLike : Lexer
letterLike = pred (> chr 160)

wordChar : Lexer
wordChar = alphaNum <|> letterLike

word : Lexer
word = some wordChar

separator : Lexer
separator = oneOf "/."

rawTokens : TokenMap Token
rawTokens =
  [ (equals, const Equals)
  , (word, Word)
  , (separator, Separator)
  , (spacesOrNewlines, const Space)
  , (comment, Comment)
  , (stringLit, \s => StringLit (pack . removeEscaped . unpack $ stripQuotes s))
  ]
  where

    stripQuotes : (str : String) -> String
    stripQuotes str = substr 1 (length str `minus` 2) str

    removeEscaped : List Char -> List Char
    removeEscaped ('\\' :: '\'' :: xs) = '\'' :: removeEscaped xs
    removeEscaped ('\\' :: '"'  :: xs) = '"' :: removeEscaped xs
    removeEscaped ('\\' :: 't'  :: xs) = '\t' :: removeEscaped xs
    removeEscaped ('\\' :: 'n'  :: xs) = '\n' :: removeEscaped xs
    removeEscaped ('\\' :: 'r'  :: xs) = '\r' :: removeEscaped xs
    removeEscaped ('\\' :: 'b'  :: xs) = '\b' :: removeEscaped xs
    removeEscaped ('\\' :: 'f'  :: xs) = '\f' :: removeEscaped xs
    removeEscaped ('\\' :: 'v'  :: xs) = '\v' :: removeEscaped xs
    removeEscaped (x            :: xs) = x    :: removeEscaped xs
    removeEscaped []                   = []

export
lex : String -> Either (Int, Int, String) (List (TokenData Token))
lex str =
  case lexTo (const False) rawTokens str of
       (tokenData, (l, c, "")) =>
         Right $ (filter (useful . tok) tokenData)
       (_, fail) => Left fail
  where
    useful : Token -> Bool
    useful (Comment c) = False
    useful Space       = False
    useful _ = True
