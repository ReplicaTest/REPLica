let Replica
  = env:REPLICA_DHALL
  ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall

let Test = Replica.Test
let Success = Replica.Status.Success

in {
   , unfortunate1 = Test :: {command = "false"}
       with status = Success
   , unfortunate2 = Test :: {command = "false"}
       with status = Success
   , unfortunate3 = Test :: {command = "false"}
       with status = Success
   , theChosen = Test :: {command = "true"}
       with status = Success
   }
