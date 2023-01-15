let toCommand = ./toCommand.dhall

let Replica = ./Type.dhall

let ReplicaDhall =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:e89a5d8a50bf5551f1012d7c627ab6d1fd278148a7341682247b2e024fcf90d4

let Test = ReplicaDhall.Test

let Command/show = ReplicaDhall.Command.show

in  \(repl : Replica) -> Test::{ command = Command/show (toCommand repl) }
