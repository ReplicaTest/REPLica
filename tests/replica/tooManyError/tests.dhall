let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:b11ac5d5195183145bbff03ba7b99e98b4e1bce32c725af5bedf01b4b328a741

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
