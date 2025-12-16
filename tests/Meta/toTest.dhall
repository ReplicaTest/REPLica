let toCommand = ./toCommand.dhall

let Replica = ./Type.dhall

let ReplicaDhall =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:83afdebd588461f9451e17d76551f27e7716774bd1ac610afe33e44b65228b28

let Test = ReplicaDhall.Test

let Command/show = ReplicaDhall.Command.show

in  \(repl : Replica) -> Test::{ command = Command/show (toCommand repl) }
