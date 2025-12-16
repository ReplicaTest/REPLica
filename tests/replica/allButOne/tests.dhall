let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:3e357131062f498832c25112266bf1c22d3c4f990c867a5d36807cd89128e8f7

let Test = Replica.Test

let Success = Replica.Status.Success

in  { unfortunate1 = Test::{ command = "false" } with status = Success
    , unfortunate2 = Test::{ command = "false" } with status = Success
    , unfortunate3 = Test::{ command = "false" } with status = Success
    , theChosen = Test::{ command = "true" } with status = Success
    }
