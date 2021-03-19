let Replica = ./dhall/replica.dhall
let Meta = ./dhall/meta.dhall

in { simplest_success = Replica.Minimal::{command = "true"}
       with description = "A call to 'true' must succeed with no output"

   , test_success = Replica.Success::{command = "true"}
        with description = Some "Check the success exit code"

   , test_failure = Replica.Failure::{command = "false"}
        with description = Some "Test a non null exit code"

   , testWorkingDir = Replica.Success::{command = "./run.sh"}
        with workingDir = Some "tests/basic"
        with description = Some "Test that the workingDir parameter is taken into account"

   , testOutput = Replica.Success::{command = "echo \"Hello, World!\""}
        with description = Some "Output is checked correctly"

   , testBefore = Replica.Success::{command = "cat new.txt"}
        with description = Some "Test that beforeTest is executed correctly"
        with workingDir = "tests/basic"
        with beforeTest = "echo \"Fresh content\" > new.txt"
        with afterTest = "rm new.txt"

   , testReplica = (Meta.replicaTest Meta.Run::{directory = "tests/replica", testFile = "empty.json"})
        with description = Some "Test that an empty test suite is passing"
        with succeed = Some True
        with tags = ["meta", "run"]

   , testOnly = (Meta.replicaTest Meta.Run::{ directory = "tests/replica"
                                            , parameters = ["--only one"]
                                            , testFile = "two.json"})
        with description = Some "Test tests filtering with \"--only\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]

   , testExclude = (Meta.replicaTest Meta.Run::{ directory = "tests/replica"
                                            , parameters = ["--exclude one"]
                                            , testFile = "two.json"})
        with description = Some "Test tests filtering with \"--exclude\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]

   , testTags = (Meta.replicaTest Meta.Run::{ directory = "tests/replica"
                                            , parameters = ["--tags shiny"]
                                            , testFile = "two.json"})
        with description = Some "Test tests filtering with \"--only\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]

   , testExcludeTags = (Meta.replicaTest Meta.Run::{ directory = "tests/replica"
                                            , parameters = ["--exclude-tags shiny"]
                                            , testFile = "two.json"})
        with description = Some "Test tests filtering with \"--exclude-tags\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]

   }
