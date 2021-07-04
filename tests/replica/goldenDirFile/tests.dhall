let Replica = https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
let Test = Replica.Test

in { valid = Test.Success :: {command = "echo \"one\" > one.txt"}
       with outputFile = Some "one.txt"
       with afterTest = ["rm one.txt"]
       with description = Some "Expectation of a file on a custom directory"
   }
