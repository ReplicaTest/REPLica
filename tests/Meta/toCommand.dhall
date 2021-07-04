let Replica = https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
let Command = Replica.Command
let toCommand
  : ./Type.dhall -> Command.Type
  = \(repl : ./Type.dhall) ->
  { executable = repl.executable
  , parameters = [repl.command] # repl.parameters # repl.testFiles
  }

in toCommand
