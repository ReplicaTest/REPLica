module Replica.Validation

%default total

public export
data ValidationM : (f : Type -> Type) -> (err, a : Type) -> Type where
  ErrorM : f err -> ValidationM f err a
  Valid : a -> ValidationM f err a

public export
implementation Functor (ValidationM f err) where
  map func (ErrorM x) = ErrorM x
  map func (Valid x) = Valid (func x)

public export
implementation Semigroup (f err) => Applicative (ValidationM f err) where
  pure x = Valid x
  (<*>) (ErrorM x) (ErrorM y) = ErrorM (x <+> y)
  (<*>) (ErrorM x) (Valid y) = ErrorM x
  (<*>) (Valid f) (ErrorM y) = ErrorM y
  (<*>) (Valid f) (Valid y) = Valid (f y)

public export
implementation (Semigroup (f err), Semigroup a) =>
               Semigroup (ValidationM f err a) where
  (<+>) (ErrorM x) (ErrorM y) = ErrorM (x <+> y)
  (<+>) (ErrorM x) (Valid y) = ErrorM x
  (<+>) (Valid x) (ErrorM y) = ErrorM y
  (<+>) (Valid x) (Valid y) = Valid (x <+> y)

public export
implementation (Semigroup (ValidationM f err a), Monoid a) =>
               Monoid (ValidationM f err a) where
  neutral = Valid neutral

public export
Validation : (err, a : Type) -> Type
Validation = ValidationM List

public export
Error : err -> Validation err a
Error = ErrorM . pure


public export
toEither : ValidationM f err a -> Either (f err) a
toEither (ErrorM x) = Left x
toEither (Valid x) = Right x
