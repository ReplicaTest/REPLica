let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:3e357131062f498832c25112266bf1c22d3c4f990c867a5d36807cd89128e8f7

let Test = Replica.Test

let to_129 = Replica.Prelude.Natural.enumerate 129

let tests
    : Replica.Type
    = Replica.Prelude.List.map
        Natural
        (Replica.Prelude.Map.Entry Text Replica.Test.Type)
        ( \(n : Natural) ->
            { mapKey = "test" ++ Replica.Prelude.Natural.show n
            , mapValue = Test::{ command = "false" }
            }
        )
        to_129

in  tests
