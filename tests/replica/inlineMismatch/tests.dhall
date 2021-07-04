let Replica = https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
let Test = Replica.Test
let Expectation = Replica.Expectation

in { mismatch = Test :: {command = "echo \"one\""}
       with description = Some "Expectation is different than one"
       with stdOut = Expectation.Exact "two\n"
   }
