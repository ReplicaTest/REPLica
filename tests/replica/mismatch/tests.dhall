let Replica = https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
let Test = Replica.Test
let Success = Replica.Status.Success

in { mismatch = Test :: {command = "echo \"one\""}
       with description = Some "Expectation is different than one"
   }
