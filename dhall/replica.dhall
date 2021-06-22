let Map = https://prelude.dhall-lang.org/v20.1.0/Map/Type.dhall

let ComplexExpectation
    : Type
    = { exact : Optional Text
      , start : Optional Text
      , end: Optional Text
      , consecutive : Optional (List Text)
      , contains : Optional (List Text)
      }

let EmptyExpectation
    = { Type = ComplexExpectation
      , default =
        { exact = None Text
        , start = None Text
        , end = None Text
        , consecutive = None (List Text)
        , contains = None (List Text)
        }
      }

let Generated = EmptyExpectation

let BySourceExpectation
    : Type
    = Map Text ComplexExpectation

let Expectation
    : Type
    = < ExactExp : Text | ContainsExp : List Text | ComplexExp : BySourceExpectation >


let Exact : Text -> Expectation = \(e : Text) -> Expectation.ExactExp e

let Contains : List Text -> Expectation
    = \(parts : List Text) ->
      Expectation.ContainsExp parts


let BySource : BySourceExpectation -> Expectation
    = \(exp : BySourceExpectation) ->  Expectation.ComplexExp exp

let StdOut : Text = "stdout"
let StdErr : Text = "stderr"

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
      , expectation : Expectation
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
        , expectation = BySource ([] : BySourceExpectation)
        , outputFile = None Text
        , pending = False
        }
      }

let Success = Minimal with default.succeed = Some True

let Failure = Minimal with default.succeed = Some False

let Replica
    : Type
    = Map Text Test

in  { Test
    , Replica
    , Minimal
    , Success
    , Failure
    , Expectation
    , Contains
    , Exact
    , Generated
    , BySource
    , BySourceExpectation
    , ComplexExpectation
    , EmptyExpectation
    , StdOut
    , StdErr
    }
