let Replica
  = env:REPLICA_DHALL
  ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall

let Test = Replica.Test

in { valid = Test :: {command = "echo \"one\""}
      with description = Some "Simple expectations"
   }
