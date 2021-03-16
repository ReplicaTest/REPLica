let Map = https://prelude.dhall-lang.org/v15.0.0/Map/Type

let Test : Type =
  { description : Optional Text
  , workingDir : Optional Text
  , beforeTest : Optional (List Text)
  , afterTest : Optional (List Text)
  , command : Text
  , succeed : Optional Bool
  }

let EmptyTest =
  { Type = Test
  , default =
    { description = None Text
    , workingDir = None Text
    , beforeTest = None (List Text)
    , afterTest = None (List Text)
    , succeed = None Bool
    }
  }

let Replica : Type = Map Text Test

let simpleTest : Text -> Test = \(cmd : Text) ->
  EmptyTest::{ command = cmd}

let successTest = \(cmd : Text) ->
  EmptyTest::{command = cmd, succeed = Some True}

let failureTest = \(cmd : Text) ->
  EmptyTest::{command = cmd, succeed = Some False}

let inDir = \(dir : Text) -> \(test : Test) ->
  test // {workingDir = dir}

in { Test
   , Replica
   , simpleTest
   , successTest
   , failureTest
   , inDir
   }
