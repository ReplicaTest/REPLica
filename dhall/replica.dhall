let Map = https://prelude.dhall-lang.org/v15.0.0/Map/Type

let Test : Type =
  { description : Optional Text
  , workingDir : Optional Text
  , tags : List Text
  , beforeTest : Optional (List Text)
  , afterTest : Optional (List Text)
  , command : Text
  , succeed : Optional Bool
  }

let Minimal =
  { Type = Test
  , default =
    { description = None Text
    , workingDir = None Text
    , tags = [] : List Text
    , beforeTest = None (List Text)
    , afterTest = None (List Text)
    , succeed = None Bool
    }
  }

let Success = Minimal with default.succeed = Some True
let Failure = Minimal with default.succeed = Some False

let Replica : Type = Map Text Test

in { Test
   , Replica
   , Minimal
   , Success
   , Failure
   }
