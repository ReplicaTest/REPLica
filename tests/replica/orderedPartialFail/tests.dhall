let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:b11ac5d5195183145bbff03ba7b99e98b4e1bce32c725af5bedf01b4b328a741

let Test = Replica.Test

let Expectation = Replica.Expectation

in  { ordered_partial_expectation_mismatch =
        Test.Success::{ command = "echo \"Hello, World!\"" }
      with description = Some "check an ordered partial expectation that fails"
      with stdOut = Expectation.Consecutive [ "World", "Hello" ]
    }
