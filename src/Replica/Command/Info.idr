module Replica.Command.Info

import Replica.Option.Types
import Replica.Option.Validate
import Replica.Other.Validation

%default total

testFile : Param String

public export
record InfoAction' (f : Type -> Type) where
  constructor MkInfo
  file : f String

public export
InfoAction : Type
InfoAction = InfoAction' Prelude.id

export
Semigroup (InfoAction' List) where
  (<+>) (MkInfo fileX)
        (MkInfo fileY)
    = MkInfo
        (fileX ++ fileY)

export
Monoid (InfoAction' List) where
  neutral = MkInfo empty

fileOption : String -> InfoAction' List -> InfoAction' List
fileOption x = record {file $= (x::)}


parseInfoOptions : List String -> InfoAction' List ->
  Validation (List String) (InfoAction' List)
parseInfoOptions [] a = Valid a
parseInfoOptions xs@(x::tail) a = do
  let Just (f, xs')
      = guard (tail == []) $> (fileOption x, tail)
    | Nothing => Error ["Unknnown option \{x}"]
  assert_total $ parseInfoOptions xs' $ f a

validateInfoAction : InfoAction' List -> Validation (List String) InfoAction
validateInfoAction nfo
  = [| MkInfo
    (one "filename" nfo.file)
    |]

export
parseInfo : List String -> Validation (List String) InfoAction
parseInfo ("info" :: xs)
  = case parseInfoOptions xs neutral of
         Valid x => validateInfoAction x
         Error e => Error e
parseInfo _ = empty


