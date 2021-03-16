let Replica = ./dhall/replica.dhall

in { simplest_success = Replica.describe
       "A call to 'true' must succeed with no output"
       (Replica.simpleTest "true")

   , test_success = Replica.describe
        "Check the success exit code"
        (Replica.successTest "true")

   , test_failure = Replica.describe
        "Test a non null exit code"
        (Replica.failureTest "false")

   , testWorkingDir = Replica.describe
        "Test that the workingDir parameter is taken into account"
        (Replica.inDir "tests/basic" (Replica.successTest "./run.sh"))
   }
