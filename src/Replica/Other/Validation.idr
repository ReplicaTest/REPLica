||| Validation: an Either-like Applicative that accumulates errors
|||
||| This module provides `Validation err a` which behaves like `Either` but with an
||| `Applicative` instance that accumulates errors using `Semigroup (<+>)` (or `Monoid`/`neutral`
||| for `Alternative`). Use it for validating multiple fields and collecting all
||| errors instead of failing fast.
|||
||| Example
||| -------
|||
|||   import Data.List
|||
|||   -- A small validator that reports an error when a value is non-positive.
|||   isPositive : Int -> Validation (List String) Int
|||   isPositive x = if x > 0 then Valid x else Error ["must be > 0"]
|||
|||   -- Combine two validations; errors from both sides will be accumulated.
|||   validateBoth : Int -> Int -> Validation (List String) (Int, Int)
|||   validateBoth a b = (,) <$> isPositive a <*> isPositive b
|||
|||   -- Example result:
|||   --   validateBoth (-1) 0 == Error ["must be > 0", "must be > 0"]
|||
||| Note: Ensure the error type has a Semigroup (or Monoid for Alternative) instance,
||| e.g., List String, so errors can be combined with <+>.
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
