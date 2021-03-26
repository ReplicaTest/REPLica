module Replica.Option.Filter

import Data.List
import Data.String

import Replica.Core
import Replica.Option.Types
import Replica.Other.Decorated

%default total

public export
record Filter' (f : Type -> Type) where
  constructor MkFilter
  only : f (List String)
  exclude : f (List String)
  onlyTags : f (List String)
  excludeTags : f (List String)
  lastFailures : f Bool

public export
Filter : Type
Filter = Done Filter'

export
TyMap Filter' where
  tyMap func x = MkFilter
    (func x.only)
    (func x.exclude)
    (func x.onlyTags)
    (func x.excludeTags)
    (func x.lastFailures)

export
TyTraversable Filter' where
  tyTraverse func x = [| MkFilter
    (func x.only)
    (func x.exclude)
    (func x.onlyTags)
    (func x.excludeTags)
    (func x.lastFailures)
    |]

export
Show Filter where
  show x = unwords
    [ "MkFilter"
    , show x.only
    , show x.exclude
    , show x.onlyTags
    , show x.excludeTags
    , show x.lastFailures
    ]

onlyPart : Part (Builder Filter') (List String)
onlyPart = inj $ MkOption
      (singleton $ MkMod (singleton "only") ['n']
          (Right $ MkValue "testX,testY" $ Just . go)
          "a comma separated list of the tests to run")
      [] compose
      where
        go : String -> List String
        go = forget . split (== ',')
        compose : List String -> Builder Filter' -> Either String (Builder Filter')
        compose xs x = case either (const []) (intersect xs) x.exclude of
          [] => Right $ record {only $= Right . (++ xs) . either (const []) id} x
          xs => Left "Some tests were both included and excluded: \{show xs}"


excludePart : Part (Builder Filter') (List String)
excludePart = inj $ MkOption
      (singleton $ MkMod (singleton "exclude") ['N']
          (Right $ MkValue "testX,testY" $ Just . go)
          "a comma separated list of the tests to exclude")
      [] compose
      where
        go : String -> List String
        go = forget . split (== ',')
        compose : List String -> Builder Filter' -> Either String (Builder Filter')
        compose xs x = case either (const []) (intersect xs) x.only of
          [] => Right $ record {exclude $= Right . (++ xs) . either (const []) id} x
          xs => Left "Some tests were both included and excluded: \{show xs}"

onlyTagsPart : Part (Builder Filter') (List String)
onlyTagsPart = inj $ MkOption
      (singleton $ MkMod ("tags" ::: ["only-tags"]) ['t']
          (Right $ MkValue "TAGS" $ Just . go)
          "a comma separated list of the tags to run")
      [] compose
      where
        go : String -> List String
        go = forget . split (== ',')
        compose : List String -> Builder Filter' -> Either String (Builder Filter')
        compose xs x = case either (const []) (intersect xs) x.excludeTags of
          [] => Right $ record {onlyTags $= Right . (++ xs) . either (const []) id} x
          xs => Left "Some tags were both included and excluded: \{show xs}"

excludeTagsPart : Part (Builder Filter') (List String)
excludeTagsPart = inj $ MkOption
      (singleton $ MkMod (singleton "exclude-tags") ['T']
          (Right $ MkValue "TAGS" $ Just . go)
          "a comma separated list of the tags to exclude")
      []
      compose
      where
        go : String -> List String
        go = forget . split (== ',')
        compose : List String -> Builder Filter' -> Either String (Builder Filter')
        compose xs x = case either (const []) (intersect xs) x.onlyTags of
          [] => Right $ record {excludeTags $= Right . (++ xs) . either (const []) id} x
          xs => Left "Some tags were both included and excluded: \{show xs}"

lastFailuresPart : Part (Builder Filter') Bool
lastFailuresPart = inj $ MkOption
      (singleton $ MkMod (singleton "last-fails") ['l']
          (Left True)
          "if a previous run fails, rerun only the tests that failed")
      False
      go
      where
        go : Bool -> Builder Filter' -> Either String (Builder Filter')
        go = ifSame lastFailures
                    (\x => record {lastFailures = Right x})
                    (const $ const "Contradictory values for last failures mode")


export
optParseFilter : OptParse (Builder Filter') (Done Filter')
optParseFilter = [|MkFilter
  (liftAp onlyPart) (liftAp excludePart)
  (liftAp onlyTagsPart) (liftAp excludeTagsPart)
  (liftAp lastFailuresPart) |]

export
defaultFilter : Default Filter'
defaultFilter = MkFilter (defaultPart onlyPart) (defaultPart excludePart)
  (defaultPart onlyTagsPart) (defaultPart excludeTagsPart)
  (defaultPart lastFailuresPart)

export
keepTest : Filter -> Test -> Bool
keepTest x y = (null x.only        || (y.name `elem` x.only))
            && (null x.exclude     || not (y.name `elem` x.exclude))
            && (null x.onlyTags    || not (null $ y.tags `intersect` x.onlyTags))
            && (null x.excludeTags || null (y.tags `intersect` x.excludeTags))
