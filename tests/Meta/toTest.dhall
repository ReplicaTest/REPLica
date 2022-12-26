let Replica
  = env:REPLICA_DHALL
  ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
let Test = Replica.Test
let Command/show = Replica.Command.show
let toCommand = ./toCommand.dhall
let Replica = ./Type.dhall

in \(repl : Replica) -> Test ::
   { command = Command/show (toCommand repl)
   }
