let Replica = ./replica.dhall

let concatSep = https://prelude.dhall-lang.org/v20.1.0/Text/concatSep

let replica_exe = "build/exec/replica"

let ReplicaTest : Type =
  { command : Text
  , parameters : List Text
  , testFile : Text
  }

let Run =
  { Type = ReplicaTest
  , default =
    { command = "run"
    , parameters = [] : List Text
    }
  }

let buildReplica : ReplicaTest -> Text = \(replica : ReplicaTest) ->
  concatSep " " [replica_exe, replica.command, concatSep " " replica.parameters, replica.testFile]

let replicaTest : ReplicaTest -> Replica.Test = \(replica : ReplicaTest) ->
  Replica.Simple::{command = buildReplica replica}

in { ReplicaTest
   , Run
   , replicaTest
   }
