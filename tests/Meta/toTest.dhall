let toCommand = ./toCommand.dhall

let Replica = ./Type.dhall

let Test = Replica.Test

let Command/show = Replica.Command.show

in  \(repl : Replica) -> Test::{ command = Command/show (toCommand repl) }
