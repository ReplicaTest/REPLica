let Replica = ../../../dhall/replica.dhall

in { valid = Replica.Success::{command = "echo \"one\" > one.txt"}
      with outputFile = Some "one.txt"
      with afterTest = ["rm one.txt"]
      with description = Some "Expectation of a file on a custom directory"
   }
