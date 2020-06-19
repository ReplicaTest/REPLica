module Replica.Options

%default total

public export
record Options where

  constructor MkOptions

  exec : String
  params : String
  input : Maybe String
  outputFile : String


defaultOutputFile : String
defaultOutputFile = "output"

mkOpt : String -> Options
mkOpt exec = MkOptions exec "" Nothing defaultOutputFile

parseParams :  Alternative f => Options -> List String -> f Options
parseParams opts ("--params" :: params :: xs)
  = parseParams (record {params = params} opts) xs
parseParams opts ("--input" :: input :: xs)
  = parseParams (record {input = pure input} opts) xs
parseParams opts ("--outputFile" :: outputFile :: xs)
  = parseParams (record {outputFile = outputFile} opts) xs
parseParams opts (x :: xs)
  = empty
parseParams opts []
  = pure opts

export
options : List String -> Maybe Options
options (e::params) = parseParams (mkOpt e) params
options _ = Nothing
