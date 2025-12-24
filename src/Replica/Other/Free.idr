||| Free applicative
module Replica.Other.Free

%default total

public export
data Ap : (Type -> Type) -> Type -> Type where
  Pure : a -> Ap f a
  MkAp : f a -> Ap f (a -> b) -> Ap f b

export
Functor (Ap f) where
  map func (Pure x) = Pure (func x)
  map func (MkAp x y) = MkAp x (map (func .) y)

export
Applicative (Ap f) where
  pure = Pure
  (<*>) (Pure y) x = map y x
  (<*>) e@(MkAp y z) x = MkAp y $ assert_smaller e (map flip z) <*> x

||| Interpret the free applicative into another Applicative by providing an interpreter for each effect `f`.
export
runAp : Applicative g => (forall x. f x -> g x) -> Ap f c -> g c
runAp func (Pure x) = pure x
runAp func (MkAp x y) = runAp func y <*> func x

||| Fold the free applicative into a Monoid, combining each interpreted effect using the Monoid operation.
export
runApM : Monoid m => (forall x. f x -> m) -> Ap f c -> m
runApM func (Pure x) = neutral
runApM func (MkAp x y) = runApM func y <+> func x

||| Variant of runApM that combines the current element before the rest (left-biased combination).
export
runApM' : Monoid m => (forall x. f x -> m) -> Ap f c -> m
runApM' func (Pure x) = neutral
runApM' func (MkAp x y) = func x <+> runApM func y

||| Lift a single effect into the free applicative structure.
export
liftAp : f a -> Ap f a
liftAp x = MkAp x (Pure id)
