let Replica = ../../../dhall/replica.dhall

in { one = Replica.Minimal::{command = "echo \"one\""}
   , two = Replica.Minimal::{command = "echo \"two\"", tags = ["shiny"]}
   }
