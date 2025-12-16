let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:3e357131062f498832c25112266bf1c22d3c4f990c867a5d36807cd89128e8f7

let Test = Replica.Test

in  { valid =
        Test::{ command = "echo \"one\"" }
      with description = Some "Simple expectations"
    }
