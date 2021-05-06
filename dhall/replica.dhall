let Map = https://prelude.dhall-lang.org/v20.1.0/Map/Type.dhall

let PartialExpectation
    : Type
    = { ordered : Bool
      , parts : List Text
      }

let Expectation
    : Type
    = < PartialExp : PartialExpectation | ExactExp : Text >

let Exact : Text -> Expectation = \(e : Text) -> Expectation.ExactExp e

let Partial : Bool -> List Text -> Expectation
    = \(ordered : Bool) ->
      \(parts : List Text) ->
      Expectation.PartialExp {ordered, parts}

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
      , spaceSensitive : Bool
      , expectation : Optional Expectation
      , outputFile : Optional Text
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
        , spaceSensitive = True
        , expectation = None Expectation
        , outputFile = None Text
        , pending = False
        }
      }

let Success = Minimal with default.succeed = Some True

let Failure = Minimal with default.succeed = Some False

let Replica
    : Type
    = Map Text Test

in  { Test, Replica, Minimal, Success, Failure, Expectation, Partial, Exact}
