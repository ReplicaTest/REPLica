let Map = https://prelude.dhall-lang.org/v20.1.0/Map/Type.dhall

let Test
    : Type
    = { description : Optional Text
      , require : List Text
      , workingDir : Optional Text
      , tags : List Text
      , beforeTest : List Text
      , afterTest : List Text
      , command : Text
      , input : Optional Text
      , succeed : Optional Bool
      , expectation : Optional Text
      , pending : Bool
      }

let Minimal =
      { Type = Test
      , default =
        { description = None Text
        , require = [] : List Text
        , workingDir = None Text
        , tags = [] : List Text
        , beforeTest = [] : List Text
        , afterTest = [] : List Text
        , input = None Text
        , succeed = None Bool
        , expectation = None Text
        , pending = False
        }
      }

let Success = Minimal with default.succeed = Some True

let Failure = Minimal with default.succeed = Some False

let Replica
    : Type
    = Map Text Test

in  { Test, Replica, Minimal, Success, Failure }
