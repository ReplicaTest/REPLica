let Replica = ./dhall/replica.dhall
let Meta = ./dhall/meta.dhall

in { simplest_success = Replica.Simple::{command = "true"}
       with description = "A call to 'true' must succeed with no output"

   , test_success = Replica.Success::{command = "true"}
        with description = Some "Check the success exit code"

   , test_failure = Replica.Failure::{command = "false"}
        with description = Some "Test a non null exit code"

   , testWorkingDir = Replica.Success::{command = "./run.sh"}
        with workingDir = Some "tests/basic"
        with description = Some "Test that the workingDir parameter is taken into account"

   , testReplica = (Meta.replicaTest Meta.Run::{testFile = "./tests/replica/empty.json"})
        with description = Some "Test that an empty test suite is passing"
        with succeed = Some True
   }
