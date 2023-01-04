let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:b11ac5d5195183145bbff03ba7b99e98b4e1bce32c725af5bedf01b4b328a741

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
