let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:e89a5d8a50bf5551f1012d7c627ab6d1fd278148a7341682247b2e024fcf90d4

let Command = Replica.Command

let toCommand
    : ./Type.dhall -> Command.Type
    = \(repl : ./Type.dhall) ->
        { executable = repl.executable
        , parameters = [ repl.command ] # repl.parameters # repl.testFiles
        }

in  toCommand
