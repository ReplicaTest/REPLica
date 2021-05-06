let Replica = ../../../dhall/replica.dhall

in { ordered_partial_expectation_mismatch =
       Replica.Success::{command = "echo \"Hello, World!\""}
         with description = Some "check an ordered partial expectation that fails"
         with expectation = Some (Replica.Partial True ["World", "Hello"])
   }
