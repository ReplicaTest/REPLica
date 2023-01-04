let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall

in  { hello = Replica.Test::{ command = "echo \"Hello, world!\"" } }
