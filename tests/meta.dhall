let Replica = ../dhall/replica.dhall

let concatSep = https://prelude.dhall-lang.org/v20.1.0/Text/concatSep

let replica_exe = "${env:PWD as Text}/build/exec/replica"

let ReplicaTest : Type =
  { command : Text
  , directory : Text
  , parameters : List Text
  , testFile : Text
  }

let Info =
  { Type = ReplicaTest
  , default =
    { command = "info"
    , parameters = [] : List Text
    }
  }

let Run =
  { Type = ReplicaTest
  , default =
    { command = "run"
    , parameters = [] : List Text
    }
  }

let buildReplica : ReplicaTest -> Text = \(replica : ReplicaTest) ->
  concatSep " " [ replica_exe
                , replica.command
                , concatSep " " replica.parameters
                , replica.testFile]

let replicaTest : ReplicaTest -> Replica.Test = \(replica : ReplicaTest) ->
  Replica.Minimal::{command = buildReplica replica}
    with workingDir = Some replica.directory

in { ReplicaTest
   , Run
   , Info
   , replicaTest
   , replica_exe
   }
