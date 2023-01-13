let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall

let Prelude = Replica.Prelude

let Test = Replica.Test

let Status = Replica.Status

let Expectation = Replica.Expectation

let start_fail =
      Test.Success::{
      , command = "echo \"Hello, World!\""
      , description = Some "Start fails"
      , stdOut = Expectation::{ start = Some "Hella" }
      }

let tests
    : Replica.Type
    = toMap { start_fail }

in  tests
