let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall

let Prelude = Replica.Prelude

let Test = Replica.Test

let Status = Replica.Status

let Expectation = Replica.Expectation

let end_failure =
      Test.Success::{
      , command = "echo \"Hello, World!\""
      , description = Some "Wrong end expectation"
      , spaceSensitive = False
      , stdOut = Expectation::{ end = Some "Warld!" }
      }

let tests
    : Replica.Type
    = toMap { end_failure }

in  tests
