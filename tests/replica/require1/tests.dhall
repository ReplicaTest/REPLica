let Replica
  = env:REPLICA_DHALL
  ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall

let Test = Replica.Test
let Success = Replica.Status.Success

let tests =
  { root_failed = Test :: {command = "false"}
      with succeed = Success
  , depends_failed = Test :: {command = "true"}
      with require = ["root_failed"]
      with succeed = Success
  }

in tests
