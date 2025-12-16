let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:3e357131062f498832c25112266bf1c22d3c4f990c867a5d36807cd89128e8f7

let Test = Replica.Test

let Status = Replica.Status

let Expectation = Replica.Expectation

let tests
    : Replica.Type
    = toMap
        { oneA = Test.Success::{
          , command = "true"
          , suite = Some "A"
          , stdOut = Expectation.Skipped
          }
        , oneB = Test.Success::{
          , command = "true"
          , suite = Some "B"
          , stdOut = Expectation.Skipped
          }
        , secondA = Test.Success::{
          , command = "true"
          , suite = Some "A"
          , stdOut = Expectation.Skipped
          }
        }

in  tests
