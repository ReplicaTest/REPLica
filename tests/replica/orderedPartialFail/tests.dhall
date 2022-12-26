let Replica
  = env:REPLICA_DHALL
  ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall

let Test = Replica.Test
let Expectation = Replica.Expectation

in { ordered_partial_expectation_mismatch =
       Test.Success :: {command = "echo \"Hello, World!\""}
         with description = Some "check an ordered partial expectation that fails"
         with stdOut = Expectation.Consecutive ["World", "Hello"]
   }
