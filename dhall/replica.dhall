let Map = https://prelude.dhall-lang.org/v20.1.0/Map/Type.dhall

let ComplexExpectationType
    : Type
    = { generated : Bool
      , exact : Optional Text
      , start : Optional Text
      , end: Optional Text
      , consecutive : List Text
      , contains : List Text
      }

let EmptyExpectation
    = { Type = ComplexExpectationType
      , default =
        { generated = False
        , exact = None Text
        , start = None Text
        , end = None Text
        , consecutive = [] : List Text
        , contains = [] : List Text
        }
      }

let Expectation
    : Type
    = < GeneratedExp : Bool
      | ExactExp : Text
      | ContainsExp : List Text
      | ComplexExp : ComplexExpectationType
      >


let Exact : Text -> Expectation = \(e : Text) -> Expectation.ExactExp e

let Contains : List Text -> Expectation
    = \(parts : List Text) ->
      Expectation.ContainsExp parts

let Consecutive : List Text -> Expectation
    = \(parts : List Text) ->
      Expectation.ComplexExp (EmptyExpectation::{consecutive = parts})

let Generated : Bool -> Expectation
    = \(b : Bool) -> Expectation.GeneratedExp b

let ComplexExpectation : ComplexExpectationType -> Expectation
    = \(exp : ComplexExpectationType) ->  Expectation.ComplexExp exp

let Status = <ExactlyStatus : Natural | SucceedStatus : Bool>

let Exactly : Natural -> Optional Status = \(t : Natural) -> Some (Status.ExactlyStatus t)
let Succeed : Bool -> Optional Status = \(t : Bool) -> Some (Status.SucceedStatus t)

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
      , status : Optional Status
      , spaceSensitive : Bool
      , stdOut : Expectation
      , stdErr : Expectation
      , files : Map Text (Expectation)
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
        , status = None Status
        , spaceSensitive = True
        , stdOut = Generated True
        , stdErr = Generated False
        , files = [] : Map Text Expectation
        , pending = False
        }
      }

let Success = Minimal with default.status = Succeed True

let Failure = Minimal with default.status = Succeed False

let Replica
    : Type
    = Map Text Test

in  { Test
    , Replica
    , Minimal
    , Success
    , Failure
    , Expectation
    , Consecutive
    , Contains
    , Exact
    , Exactly
    , Succeed
    , Generated
    , ComplexExpectation
    , ComplexExpectationType
    , EmptyExpectation
    }
