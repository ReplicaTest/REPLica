let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:3e357131062f498832c25112266bf1c22d3c4f990c867a5d36807cd89128e8f7

let Test = Replica.Test

in  { valid =
        Test.Success::{ command = "echo \"one\" > one.txt" }
      with outputFile = Some "one.txt"
      with afterTest = [ "rm one.txt" ]
      with description = Some "Expectation of a file on a custom directory"
    }
