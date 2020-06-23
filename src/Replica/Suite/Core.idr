module Replica.Suite.Core

%default total

public export
record SuiteM (f : Type -> Type) where
  constructor MkSuite

  name : f String
  tests : f (List String)

public export
Suite : Type
Suite = SuiteM id

public export
SuiteBuilder : Type
SuiteBuilder = SuiteM List

emptySuite : SuiteBuilder
emptySuite = MkSuite empty empty

public export
implementation Semigroup SuiteBuilder where
  (<+>) x y = MkSuite (x.name <+> y.name) (x.tests <+> y.tests)
