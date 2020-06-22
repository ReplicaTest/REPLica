module Replica.Options

%default total

public export
record Options where

  constructor MkOptions

  interactive : Bool
  tests : List String


defaultOutputFile : String
defaultOutputFile = "output"

mkOpt : Options
mkOpt = MkOptions False []

parseParams :  Options -> List String -> Options
parseParams opts ("--interactive" :: xs)
  = parseParams (record {interactive = True} opts) xs
parseParams opts (x :: xs)
  = parseParams (record {tests $= (++ [x])} opts) xs
parseParams opts []
  = opts

export
options : List String -> Options
options params = parseParams mkOpt params

