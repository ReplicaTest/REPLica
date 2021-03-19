let Map = https://prelude.dhall-lang.org/v20.1.0/Map/Type

let Test : Type =
  { description : Optional Text
  , workingDir : Optional Text
  , beforeTest : Optional (List Text)
  , afterTest : Optional (List Text)
  , command : Text
  , succeed : Optional Bool
  }

let Simple =
  { Type = Test
  , default =
    { description = None Text
    , workingDir = None Text
    , beforeTest = None (List Text)
    , afterTest = None (List Text)
    , succeed = None Bool
    }
  }

let Success = Simple with default.succeed = Some True
let Failure = Simple with default.succeed = Some False

let Replica : Type = Map Text Test

in { Test
   , Replica
   , Simple
   , Success
   , Failure
   }
