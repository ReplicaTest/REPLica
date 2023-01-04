let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:b11ac5d5195183145bbff03ba7b99e98b4e1bce32c725af5bedf01b4b328a741

let Command = Replica.Command

let toCommand
    : ./Type.dhall -> Command.Type
    = \(repl : ./Type.dhall) ->
        { executable = repl.executable
        , parameters = [ repl.command ] # repl.parameters # repl.testFiles
        }

in  toCommand
