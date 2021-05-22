module Replica.Other.Validation

%default total

public export
data Validation : (err, a : Type) -> Type where
  Valid : (x : a) -> Validation err a
  Error : (e : err) -> Validation err a

export
Functor (Validation err) where
  map f (Valid x) = Valid $ f x
  map f (Error x) = Error x

export
(Semigroup err) => Applicative (Validation err) where
  pure = Valid
  (Valid f)  <*> (Valid x)  = Valid $ f x
  (Valid _)  <*> (Error e)  = Error e
  (Error e)  <*> (Valid x)  = Error e
  (Error e1) <*> (Error e2) = Error $ e1 <+> e2

export
(Monoid err) => Alternative (Validation err) where
  empty = Error neutral
  (<|>) (Valid x) y = Valid x
  (<|>) (Error e) (Valid y) = Valid y
  (<|>) (Error e) (Error r) = Error $ e <+> r

export
fromEither : Applicative f =>  Either err a -> Validation (f err) a
fromEither = either (Error . pure) Valid

export
toMaybe : Validation _ a -> Maybe a
toMaybe (Valid x) = Just x
toMaybe (Error e) = Nothing
