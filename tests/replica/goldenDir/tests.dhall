let Replica = ../../../dhall/replica.dhall

in { valid = Replica.Minimal::{command = "echo \"one\""}
      with description = Some "Simple expectations"
   }
