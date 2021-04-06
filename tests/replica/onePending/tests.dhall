let Replica = ../../../dhall/replica.dhall

in { one = Replica.Minimal::{command = "echo \"one\""}
       with pending = True
   , two = Replica.Minimal::{command = "echo \"two\""}
   }

