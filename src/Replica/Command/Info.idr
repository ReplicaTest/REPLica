module Replica.Command.Info

import Replica.Option.Types
import Replica.Option.Validate
import Replica.Other.Validation

%default total

testFile : Param String
testFile = MkParam "filename" Just

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

fileOption : String -> InfoAction' List
fileOption x = record {file = [x]} (neutral {ty = InfoAction' List})


parseInfoOptions : List String -> Validation (List String) (InfoAction' List)
parseInfoOptions xs =
  either
    (\x => Error ["Unknnown option \{x}"])
    Valid
    $ parse [ inj $ map fileOption testFile ] xs

validateInfoAction : InfoAction' List -> Validation (List String) InfoAction
validateInfoAction nfo = [| MkInfo (one "filename" nfo.file) |]

export
parseInfo : List String -> Validation (List String) InfoAction
parseInfo ("info" :: xs)
  = case parseInfoOptions xs of
         Valid x => validateInfoAction x
         Error e => Error e
parseInfo _ = empty


