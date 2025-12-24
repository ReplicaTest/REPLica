||| `Validation` an either applicative that accumulates error
module Replica.Other.Validation

%default total

-- | `Validation` behaves like an `Either` that can accumulate errors.
-- | `Valid' holds a successful value; `Error` holds errors which can be combined.
public export
data Validation : (err, a : Type) -> Type where
  Valid : (x : a) -> Validation err a
  Error : (e : err) -> Validation err a

-- | Functor instance applies a function to a `Valid` value and leaves `Error` untouched.
export
Functor (Validation err) where
  map f (Valid x) = Valid $ f x
  map f (Error x) = Error x

-- | `Applicative` accumulates errors when both operands are `Error` using `Semigroup (<+>)`.
export
(Semigroup err) => Applicative (Validation err) where
  pure = Valid
  (Valid f)  <*> (Valid x)  = Valid $ f x
  (Valid _)  <*> (Error e)  = Error e
  (Error e)  <*> (Valid x)  = Error e
  (Error e1) <*> (Error e2) = Error $ e1 <+> e2

-- | `Alternative` provides a choice: prefers the first `Valid` result, combining errors when both fail.
export
(Monoid err) => Alternative (Validation err) where
  empty = Error neutral
  (<|>) (Valid x) y = Valid x
  (<|>) (Error e) (Valid y) = Valid y
  (<|>) (Error e) (Error r) = Error $ e <+> r

-- | `fromEither` lifts an `Either` into Validation, wrapping the `Left` error into the provided `Applicative`.
export
fromEither : Applicative f =>  Either err a -> Validation (f err) a
fromEither = either (Error . pure) Valid

-- | toMaybe converts a `Validation` into a `Maybe`, discarding any errors.
export
toMaybe : Validation _ a -> Maybe a
toMaybe (Valid x) = Just x
toMaybe (Error e) = Nothing
