module Replica.Other.Decorated

import Replica.Option.Types
import Replica.Other.Free


%default total

public export
Builder : (ty : (Type -> Type) -> Type) -> Type
Builder ty = ty (\a => Either (Maybe a) a)


public export
Default : (ty : (Type -> Type) -> Type) -> Type
Default ty = ty Maybe

public export
Done : (ty : (Type -> Type) -> Type) -> Type
Done ty = ty id

public export
interface TyMap (0 ty : (Type -> Type) -> Type) where
  tyMap : (forall x. f x -> g x) -> ty f -> ty g

public export
interface TyTraversable (0 ty : (Type -> Type) -> Type) where
  tyTraverse : Applicative g => (forall a. f a -> g a) -> ty f -> g (Done ty)

export
initBuilder : TyMap ty => Default ty -> Builder ty
initBuilder = tyMap Left

export
build : TyTraversable ty => Builder ty -> Maybe (Done ty)
build = tyTraverse (either id Just)

export
one : (get : b -> Either c a) -> (set : a -> b -> b) -> (errMsg : a -> a -> err) ->
      a -> b -> Either err b
one get set errMsg x y = case get y of
                              Left _ => Right $ set x y
                              Right z => Left $ errMsg x z

export
ifSame : Eq a =>
         (get : b -> Either c a) -> (set : a -> b -> b) -> (errMsg : a -> a -> err) ->
         a -> b -> Either err b
ifSame get set errMsg x y = case get y of
  Left  _ => Right $ set x y
  Right z => if x /= z
                then Left $ errMsg x z
                else Right y

export
first : (get : b -> Either c a) -> (set : a -> b -> b) -> a -> b -> b
first get set x y = case get y of
  Left _ => set x y
  Right _ => y
