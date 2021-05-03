let Replica = ../../../dhall/replica.dhall

in { mismatch = Replica.Minimal::{command = "echo \"one\""}
      with description = Some "Expectation is different than one"
      with expectation = Some "two\n"
   }
