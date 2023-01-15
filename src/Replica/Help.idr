||| Help building consistent help for a command line interface
module Replica.Help

import Data.List
import Data.List1
import Data.String
import Data.String.Extra
import Replica.Other.String

%default total

public export
record Help where
  constructor MkHelp
  name : String
  usage : Maybe String
  description : String
  chapter : List (String, List1 Help)
  lastWords : Maybe String

padRightTo : Nat -> String -> String
padRightTo k x = x ++ pack (replicate (minus k (length x)) ' ')

entrySynopsis : Nat -> Help -> List String
entrySynopsis k x =
  let (y::ys) = lines x.description
    | [] => []
  in "\{padRightTo k x.name}  \{y}" :: map (pack (replicate (2 + k) ' ') ++) ys

chapterSynopsis : Nat -> String -> List1 Help -> String
chapterSynopsis k x xs = removeTrailingNL $ unlines $
  "\{x}:" :: map (withOffset 2) (forget xs >>= entrySynopsis k)

export
display : Help -> String
display h = removeTrailingNL $ unlines $ "" :: intersperse "" (
  maybe id (\u => ("Usage: \{u}" ::)) h.usage $
  h.description ::
  map (uncurry $ chapterSynopsis maxLengthName) h.chapter
  ++ (maybe [] (\l => [l]) h.lastWords))
  where
    maxLengthName : Nat
    maxLengthName = foldl (\x, h => max x (length h.name)) 0 (h.chapter >>= forget . snd)
