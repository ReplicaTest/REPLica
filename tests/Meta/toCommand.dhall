let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:83afdebd588461f9451e17d76551f27e7716774bd1ac610afe33e44b65228b28

let Command = Replica.Command

let toCommand
    : ./Type.dhall -> Command.Type
    = \(repl : ./Type.dhall) ->
        { executable = repl.executable
        , parameters = [ repl.command ] # repl.parameters # repl.testFiles
        }

in  toCommand
