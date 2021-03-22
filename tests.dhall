let Map = https://prelude.dhall-lang.org/v15.0.0/Map/Type
let Replica = ./dhall/replica.dhall
let Meta = ./dhall/meta.dhall

let tests =
   { simplest_success = Replica.Minimal::{command = "true"}
        with description = Some "A call to 'true' must succeed with no output"

   , test_success = Replica.Success::{command = "true"}
        with description = Some "Check the success exit code"

   , test_failure = Replica.Failure::{command = "false"}
        with description = Some "Test a non null exit code"

   , testWorkingDir = Replica.Success::{command = "./run.sh"}
        with description = Some "Test that the workingDir parameter is taken into account"
        with workingDir = Some "tests/basic"

   , testOutput = Replica.Success::{command = "echo \"Hello, World!\""}
        with description = Some "Output is checked correctly"

   , testBefore = Replica.Success::{command = "cat new.txt"}
        with description = Some "Test that beforeTest is executed correctly"
        with workingDir = Some "tests/basic"
        with beforeTest = ["echo \"Fresh content\" > new.txt"]
        with afterTest = ["rm new.txt"]
        with tags = ["beforeTest"]

   , testReplica =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/empty"
                                    , testFile = "tests.json"})
        with description = Some "Test that an empty test suite is passing"
        with succeed = Some True
        with tags = ["meta", "run"]

   , testOnly =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                    , parameters = ["--only one"]
                                    , testFile = "tests.json"})
        with description = Some "Test tests filtering with \"--only\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]

   , testExclude =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                    , parameters = ["--exclude one"]
                                    , testFile = "tests.json"})
        with description = Some "Test tests filtering with \"--exclude\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]

   , testTags =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                    , parameters = ["--tags shiny"]
                                    , testFile = "tests.json"})
        with description = Some "Test tests filtering with \"--only\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]

   , testExcludeTags = (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                                   , parameters = ["--exclude-tags shiny"]
                                                   , testFile = "tests.json"})
        with description = Some "Test tests filtering with \"--exclude-tags\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]

   , testRequire =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/require1"
                                    , testFile = "tests.json"})
        with description = Some "A test failed when its requirements failed"
        with succeed = Some False
        with tags = ["meta", "require", "run"]

   , testSkipExculdedDependencies =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/require1"
                                    , parameters = ["--only depends_failed"]
                                    , testFile = "tests.json"})
        with description = Some "Dependencies that aren't included in the selected tests are ignored"
        with succeed = Some True
        with tags = ["meta", "require", "run"]

   , testPunitive =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/allButOne"
                                    , parameters = ["--punitive"]
                                    , testFile = "tests.json"})
        with description = Some "Test the punitive mode"
        with succeed = Some False
        with tags = ["punitive", "meta", "run"]

   , testBeforeTestFailImpact =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/beforeFailed"
                                    , testFile = "tests.json"})
        with description = Some "we should be able to fallback to a normal behaviour after a failure of beforeTest"
        with tags = ["beforeTest"]
   }

let testsCheck : Map Text Replica.Test = toMap tests

in tests
