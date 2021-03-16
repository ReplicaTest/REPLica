let Replica = ./dhall/replica.dhall

in { simplest_success = Replica.simpleTest "true"
   , test_success = Replica.successTest "true"
   , test_failure = Replica.failureTest "false"
   , testWorkingDir = Replica.inDir "tests/basic" (Replica.successTest "./run.sh")
   }
