let Replica = ./dhall/replica.dhall
let Meta = ./tests/meta.dhall

let parsing_errors = ./tests/parsing_errors.dhall
let idris = ./tests/idris.dhall

let tests : Replica.Replica = [
   { mapKey = "simplest_success"
   , mapValue =
        Replica.Minimal::{command = "true"}
        with description = Some "A call to 'true' must succeed with no output"
   },
   { mapKey = "test_success"
   , mapValue =
        Replica.Success::{command = "true"}
        with description = Some "Check the success exit code"
   },
   { mapKey = "test_failure"
   , mapValue =
        Replica.Failure::{command = "false"}
        with description = Some "Test a non null exit code"
   },
   { mapKey = "test_given_expectation"
   , mapValue = Replica.Success::{command = "echo \"Hello, world!\""}
        with description = Some "We use the given expectation field if it exists"
        with expectation = Some  (Replica.Exact "Hello, world!\n")
   },
   { mapKey = "testWorkingDir"
   , mapValue =
        Replica.Success::{command = "./run.sh"}
        with description = Some "Test that the workingDir parameter is taken into account"
        with workingDir = Some "tests/basic"
   },
   { mapKey = "testOutput"
   , mapValue =
        Replica.Success::{command = "echo \"Hello, World!\""}
        with description = Some "Output is checked correctly"
   },
   { mapKey = "testBefore"
   , mapValue =
        Replica.Success::{command = "cat new.txt"}
        with description = Some "Test that beforeTest is executed correctly"
        with workingDir = Some "tests/basic"
        with beforeTest = ["echo \"Fresh content\" > new.txt"]
        with afterTest = ["rm new.txt"]
        with tags = ["beforeTest"]
   },
   { mapKey = "testReplica"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/empty"
                                    , testFile = "tests.json"})
        with description = Some "Test that an empty test suite is passing"
        with succeed = Some True
        with tags = ["meta", "run"]

   },
   { mapKey = "testOutputMismatch"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/mismatch"
                                    , testFile = "tests.json"})
        with description = Some "Content mismatch is printed on error"
        with succeed = Some False
        with tags = ["meta", "run"]
   },
   { mapKey = "testOnly"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                    , parameters = ["--only one"]
                                    , testFile = "tests.json"})
        with description = Some "Test tests filtering with \"--only\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]
   },
   { mapKey = "testExclude"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                    , parameters = ["--exclude one"]
                                    , testFile = "tests.json"})
        with description = Some "Test tests filtering with \"--exclude\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]
   },
   { mapKey = "testTags"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                    , parameters = ["--tags shiny"]
                                    , testFile = "tests.json"})
        with description = Some "Test tests filtering with \"--only\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]
   },
   { mapKey = "testExcludeTags"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                    , parameters = ["--exclude-tags shiny"]
                                    , testFile = "tests.json"})
        with description = Some "Test tests filtering with \"--exclude-tags\""
        with succeed = Some True
        with tags = ["filter", "meta", "run"]
   },
   { mapKey = "testRequire"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/require1"
                                    , testFile = "tests.json"})
        with description = Some "A test failed when its requirements failed"
        with succeed = Some False
        with tags = ["meta", "require", "run"]

   },
   { mapKey = "testSkipExcludedDependencies"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/require1"
                                    , parameters = ["--only depends_failed"]
                                    , testFile = "tests.json"})
        with description = Some "Dependencies that aren't included in the selected tests are ignored"
        with succeed = Some True
        with tags = ["meta", "require", "run"]
   },
   { mapKey = "testPunitive"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/allButOne"
                                    , parameters = ["--punitive"]
                                    , testFile = "tests.json"})
        with description = Some "Test the punitive mode"
        with succeed = Some False
        with tags = ["punitive", "meta", "run"]
   },
   { mapKey = "testPending"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/onePending"
                                    , testFile = "tests.json"})
        with description = Some "Test that pending tests aren't processed"
        with succeed = Some True
        with tags = ["pending", "meta", "run"]
   },
   { mapKey = "testBeforeTestFailImpact"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/beforeFailed"
                                    , testFile = "tests.json"})
        with description = Some "we should be able to fallback to a normal behaviour after a failure of beforeTest"
        with tags = ["beforeTest"]
   },
   { mapKey = "testInput"
   , mapValue =
       Replica.Success::{command = "cat", input = Some "hello, world"}
        with description = Some "pass input to the command"
   },
   { mapKey = "check_file"
   , mapValue =
       Replica.Success::{command = "echo \"hello, world\" > test.blurb"}
        with outputFile = Some "test.blurb"
        with afterTest = ["rm test.blurb"]
        with description = Some "pass input to the command"
   },
   { mapKey = "no_golden_with_inlined_expectation"
   , mapValue =
        (Meta.replicaTest Meta.Run::{ directory = "tests/replica/inlineMismatch"
                                    , parameters = [ "--interactive" ]
                                    , testFile = "tests.json"})
        with description = Some "No interaction for mismatch when a value is given"
        with require = [ "testOutputMismatch" ]
        with succeed = Some False
        with tags = ["meta", "run"]
   },
   { mapKey = "ordered_partial_expectation_match"
   , mapValue =
       Replica.Success::{command = "echo \"Hello, World!\""}
         with description = Some "check a partial expectation that succeeds"
         with expectation = Some (Replica.Partial True ["Hello", "World"])
         with tags = ["expectation", "run", "partial"]
   },
   { mapKey = "whatever_partial_expectation_match"
   , mapValue =
       Replica.Success::{command = "echo \"Hello, World!\""}
         with description = Some "check a not ordered partial expectation that succeeds"
         with expectation = Some (Replica.Partial False ["World", "Hello"])
         with tags = ["expectation", "run", "partial"]
   },
   { mapKey = "ordered_partial_expectation_mismatch"
   , mapValue =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/orderedPartialFail"
                                   , testFile = "tests.json"})
         with description = Some "check an ordered partial expectation that fails"
         with succeed = Some False
         with tags = ["expectation", "run", "partial"]
   },
   { mapKey = "custom_golden_dir"
   , mapValue =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/goldenDir"
                                   , parameters = ["--golden-dir golden"]
                                   , testFile = "tests.json"})
         with description = Some "test --goldenDir"
         with succeed = Some True
         with tags = ["config", "golden", "meta"]
   },
   { mapKey = "custom_golden_dir_for_file"
   , mapValue =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/goldenDirFile"
                                   , parameters = ["--golden-dir golden"]
                                   , testFile = "tests.json"})
         with require = [ "check_file" ]
         with description = Some "test --goldenDir for files"
         with succeed = Some True
         with tags = ["config", "golden", "meta"]
   },
   { mapKey = "space_unsensitive_ordered_partial_expectation_match"
   , mapValue =
       Replica.Success::{command = "echo \"Hello, World!\""}
         with spaceSensitive = False
         with description = Some "check a space unsensitive partial expectation that succeeds"
         with expectation = Some (Replica.Partial True ["Hello ", " World "])
         with tags = ["expectation", "run", "partial", "space"]
   }
   ]

in tests # parsing_errors # idris
