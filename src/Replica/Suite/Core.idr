module Replica.Suite.Core

import public Replica.Path

%default total

public export
record SuiteM (f : Type -> Type) where
  constructor MkSuite

  tests : f (List Path)

public export
Suite : Type
Suite = SuiteM id

public export
SuiteBuilder : Type
SuiteBuilder = SuiteM List

public export
emptyBuilder : SuiteBuilder
emptyBuilder = MkSuite empty

public export
implementation Semigroup SuiteBuilder where
  (<+>) x y = MkSuite (x.tests <+> y.tests)

public export
implementation Monoid SuiteBuilder where
  neutral = emptyBuilder

public export
data BuildError = InvalidTestPath String


public export
build : SuiteBuilder -> Suite
build (MkSuite tests) = MkSuite $ concat tests
