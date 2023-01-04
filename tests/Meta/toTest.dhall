let toCommand = ./toCommand.dhall

let Replica = ./Type.dhall

let ReplicaDhall =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:b11ac5d5195183145bbff03ba7b99e98b4e1bce32c725af5bedf01b4b328a741

let Test = ReplicaDhall.Test

let Command/show = ReplicaDhall.Command.show

in  \(repl : Replica) -> Test::{ command = Command/show (toCommand repl) }
