module Replica.TestConfig.Core

import public Data.List
import Data.Strings

public export
Path : Type
Path = DPair (List String) NonEmpty


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
data BuildError = CantBuild

defaultOutput : String
defaultOutput = "output"

public export
emptyBuilder : BuildTestConfig
emptyBuilder = MkTestConfig [] [] [] [] []

public export
implementation Semigroup BuildTestConfig where
  (<+>) x y
    = MkTestConfig
        (x.exec <+> y.exec)
        (x.path <+> y.path)
        (x.params <+> y.params)
        (x.inputFile <+> y.inputFile)
        (x.outputFile <+> y.outputFile)

public export
implementation Monoid BuildTestConfig where
  neutral = emptyBuilder

public export
build : BuildTestConfig -> Either BuildError TestConfig
build (MkTestConfig [exec] [path] params mInput mOutputFile) = do
  let Just inputFile = case mInput of
                        [] => Just Nothing
                        [x] => Just x
                        _ => Nothing
    | _ => Left CantBuild
  let Just outputFile = case mOutputFile of
                             [] => Just defaultOutput
                             [x] => Just x
                             _ => Nothing
    | _ => Left CantBuild
  pure $ MkTestConfig exec path (unwords params) inputFile outputFile
build _ = Left CantBuild
