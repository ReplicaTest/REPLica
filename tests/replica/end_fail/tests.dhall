let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:3e357131062f498832c25112266bf1c22d3c4f990c867a5d36807cd89128e8f7

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
