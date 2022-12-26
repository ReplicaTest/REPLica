let Replica
  = env:REPLICA_DHALL
  ? https://raw.githubusercontent.com/ReplicaTest/REPLica/main/dhall/replica.dhall

in { hello = Replica.Minimal::{command = "echo \"Hello, world!\""}}
