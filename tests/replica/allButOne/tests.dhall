let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:b11ac5d5195183145bbff03ba7b99e98b4e1bce32c725af5bedf01b4b328a741

let Test = Replica.Test

let Success = Replica.Status.Success

in  { unfortunate1 = Test::{ command = "false" } with status = Success
    , unfortunate2 = Test::{ command = "false" } with status = Success
    , unfortunate3 = Test::{ command = "false" } with status = Success
    , theChosen = Test::{ command = "true" } with status = Success
    }
