module Replica.TestConfig.Core

import public Data.List
import Data.Strings

import public Replica.Path
import public Replica.Validation

public export
record TestConfigM (m : Type -> Type) where

  constructor MkTestConfig

  exec : m String
  path : m Path
  params : m String
  inputFile : m (Maybe String)
  outputFile : m String

public export
TestConfig : Type
TestConfig = TestConfigM id

public export
BuildTestConfig : Type
BuildTestConfig = TestConfigM List

public export
data BuildError
   = TooManyExec (List String)
   | TooManyPath (List Path)
   | InvalidPath a
   | TooManyInputFiles (List String)
   | TooManyOutputFiles (List String)
   | MissingPah
   | MissingExec

defaultOutput : String
defaultOutput = "output"

public export
emptyBuilder : BuildTestConfig
emptyBuilder = MkTestConfig [] [] [] [] []

public export
implementation Semigroup BuildTestConfig where
  (<+>) x y = MkTestConfig
        (x.exec <+> y.exec)
        (x.path <+> y.path)
        (x.params <+> y.params)
        (x.inputFile <+> y.inputFile)
        (x.outputFile <+> y.outputFile)

public export
implementation Monoid BuildTestConfig where
  neutral = emptyBuilder

public export
build : BuildTestConfig -> Validation BuildError TestConfig
build (MkTestConfig exec path params mInput mOutput)
  = MkTestConfig
  <$> case exec of
           [] => Error MissingExec
           [x] => pure x
           xs => Error $ TooManyExec xs
  <*> case path of
           [] => Error MissingPah
           [x] => pure x
           xs => Error $ TooManyPath xs
  <*> pure (unwords params)
  <*> case mapMaybe id mInput of
           [] => pure Nothing
           [x] => pure $ Just x
           xs => Error $ TooManyInputFiles xs
  <*> case mOutput of
           [] => pure defaultOutput
           [x] => pure x
           xs => Error $ TooManyOutputFiles xs
