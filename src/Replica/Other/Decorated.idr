||| Decorate a data structure with a higher-kinded type to represent defaulted, built, or in-progress states.
module Replica.Other.Decorated

import Replica.Option.Types
import Replica.Other.Free


%default total

-- | Builder wraps a higher-kinded type `ty` specialized to `Either (Maybe a) a`.
-- | The `Left` branch holds the `Default` value; `Right` holds an explicitly provided value.
public export
Builder : (ty : (Type -> Type) -> Type) -> Type
Builder ty = ty (\a => Either (Maybe a) a)


-- | Default specializes `ty` to Maybe, representing fields with default/optional values.
public export
Default : (ty : (Type -> Type) -> Type) -> Type
Default ty = ty Maybe

-- | Done specializes `ty` to id, representing a fully-built structure with concrete values.
public export
Done : (ty : (Type -> Type) -> Type) -> Type
Done ty = ty id

-- | TyMap provides a natural transformation mapping over the higher-kinded container `ty`.
public export
interface TyMap (0 ty : (Type -> Type) -> Type) where
  tyMap : (forall x. f x -> g x) -> ty f -> ty g

-- | TyTraversable allows traversing `ty` with an Applicative effect, collecting results into `Done ty`.
public export
interface TyTraversable (0 ty : (Type -> Type) -> Type) where
  tyTraverse : Applicative g => (forall a. f a -> g a) -> ty f -> g (Done ty)

-- | initBuilder converts a `Default ty` into a `Builder ty by` wrapping defaults into the `Left` of `Either`.
export
initBuilder : TyMap ty => Default ty -> Builder ty
initBuilder = tyMap Left

-- | build attempts to construct a `Done ty` from a `Builder ty`.
-- | Returns Nothing if any field is missing; otherwise returns Just the completed structure.
export
build : TyTraversable ty => Builder ty -> Maybe (Done ty)
build = tyTraverse (either id Just)

-- | one sets a single field using `set` if the field is not already set.
-- | If the getter reports the field is already set (Right z), returns Left with an error from `errMsg`.
export
one : (get : b -> Either c a) -> (set : a -> b -> b) -> (errMsg : a -> a -> err) ->
      a -> b -> Either err b
one get set errMsg x y = case get y of
                              Left _ => Right $ set x y
                              Right z => Left $ errMsg x z

-- | ifSame sets the field to `x` only if it is unset or already equal to `x`.
-- | If the field is set to a different value, returns Left with an error from `errMsg`.
export
ifSame : Eq a =>
         (get : b -> Either c a) -> (set : a -> b -> b) -> (errMsg : a -> a -> err) ->
         a -> b -> Either err b
ifSame get set errMsg x y = case get y of
  Left  _ => Right $ set x y
  Right z => if x /= z
                then Left $ errMsg x z
                else Right y

-- | first sets the field to `x` if it is unset; otherwise leaves the structure unchanged.
export
first : (get : b -> Either c a) -> (set : a -> b -> b) -> a -> b -> b
first get set x y = case get y of
  Left _ => set x y
  Right _ => y
