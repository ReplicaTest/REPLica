let Replica = ./dhall/replica.dhall
let Meta = ./tests/meta.dhall

let parsing_errors = ./tests/parsing_errors.dhall
let idris = ./tests/idris.dhall

let tests : Replica.Replica = toMap
   { simplest_success = Replica.Minimal::
       { command = "true"
       , description = Some "A call to 'true' must succeed with no output"
       }
   , test_success = Replica.Success::
       { command = "true"
       , description = Some "Check the success exit code"
       }
   , test_failure = Replica.Failure::
       { command = "false"
       , description = Some "Test a non null exit code"
       }
   , test_given_expectation = Replica.Success::
       { command = "echo \"Hello, world!\""
       , description = Some "We use the given expectation field if it exists"
       , expectation = Replica.Exact "Hello, world!\n"
       }
   , testWorkingDir = Replica.Success::
       { command = "./run.sh"
       , description = Some "Test that the workingDir parameter is taken into account"
       , workingDir = Some "tests/basic"
       }
   , testOutput = Replica.Success::
       { command = "echo \"Hello, World!\""
       , description = Some "Output is checked correctly"
       }
   , testBefore = Replica.Success::
       { command = "cat new.txt"
       , description = Some "Test that beforeTest is executed correctly"
       , workingDir = Some "tests/basic"
       , beforeTest = ["echo \"Fresh content\" > new.txt"]
       , afterTest = ["rm new.txt"]
       , tags = ["beforeTest"]
       }
   , testReplica =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/empty"
                                   , testFile = "tests.json"})
          with description = Some "Test that an empty test suite is passing"
          with succeed = Some True
          with tags = ["meta", "run"]

   , testOutputMismatch =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/mismatch"
                                   , testFile = "tests.json"})
          with description = Some "Content mismatch is printed on error"
          with succeed = Some False
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
   , testExcludeTags =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
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

   , testSkipExcludedDependencies =
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
   , testPending =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/onePending"
                                   , testFile = "tests.json"})
          with description = Some "Test that pending tests aren't processed"
          with succeed = Some True
          with tags = ["pending", "meta", "run"]
   , testBeforeTestFailImpact =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/beforeFailed"
                                   , testFile = "tests.json"})
          with description = Some "we should be able to fallback to a normal behaviour after a failure of beforeTest"
          with tags = ["beforeTest"]
   , testInput = Replica.Success::
       { command = "cat", input = Some "hello, world"
       , description = Some "pass input to the command"
       }
   , check_file = Replica.Success::
       { command = "echo \"hello, world\" > test.blurb"
       , outputFile = Some "test.blurb"
       , afterTest = ["rm test.blurb"]
       , description = Some "pass input to the command"
       }
   , no_golden_with_inlined_expectation =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/inlineMismatch"
                                   , parameters = [ "--interactive" ]
                                   , testFile = "tests.json"})
          with description = Some "No interaction for mismatch when a value is given"
          with require = [ "testOutputMismatch" ]
          with succeed = Some False
          with tags = ["meta", "run"]
   , ordered_partial_expectation_match = Replica.Success::
       { command = "echo \"Hello, World!\""
       , description = Some "check a partial expectation that succeeds"
       , expectation = Replica.BySource ( toMap
           { stdOut = Replica.EmptyExpectation::{consecutive = ["Hello", "World"]}
           }
         )
       , tags = ["expectation", "run", "partial"]
       }
   , whatever_partial_expectation_match = Replica.Success::
       { command = "echo \"Hello, World!\""
       , description = Some "check a not ordered partial expectation that succeeds"
       , expectation = Replica.BySource (toMap
           {stdOut = Replica.EmptyExpectation::{contains = ["World", "Hello"]}
           })
       , tags = ["expectation", "run", "partial"]
       }
   , ordered_partial_expectation_mismatch =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/orderedPartialFail"
                                  , testFile = "tests.json"})
           with description = Some "check an ordered partial expectation that fails"
           with succeed = Some False
           with tags = ["expectation", "run", "partial"]
   , custom_golden_dir =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/goldenDir"
                                  , parameters = ["--golden-dir golden"]
                                  , testFile = "tests.json"})
           with description = Some "test --goldenDir"
           with succeed = Some True
           with tags = ["config", "golden", "meta"]
   , custom_golden_dir_for_file =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/goldenDirFile"
                                  , parameters = ["--golden-dir golden"]
                                  , testFile = "tests.json"})
           with require = [ "check_file" ]
           with description = Some "test --goldenDir for files"
           with succeed = Some True
           with tags = ["config", "golden", "meta"]
   , space_unsensitive_ordered_partial_expectation_match = Replica.Success::
       { command = "echo \"Hello, World!\""
       , spaceSensitive = False
       , description = Some "check a space unsensitive partial expectation that succeeds"
       , expectation = Replica.BySource (toMap
           {stdOut = Replica.EmptyExpectation::{consecutive = ["Hello ", " World "]}})
       , tags = ["expectation", "run", "partial", "space"]
       }
   , local_config =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/localConfig"
                                    , testFile = "tests.json"})
           with require = [ "custom_golden_dir" ]
           with description = Some "test localConfig"
           with succeed = Some True
           with tags = ["config", "local", "meta"]
   }

in tests # parsing_errors # idris
