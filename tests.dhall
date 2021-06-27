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
       , stdOut = Replica.Exact "Hello, world!\n"
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
          with status = Replica.Succeed True
          with tags = ["meta", "run"]
   , testOutputMismatch =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/mismatch"
                                   , testFile = "tests.json"})
          with description = Some "Content mismatch is printed on error"
          with status = Replica.Succeed False
          with tags = ["meta", "run"]
   , testOnly =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                   , parameters = ["--only one"]
                                   , testFile = "tests.json"})
          with description = Some "Test tests filtering with \"--only\""
          with status = Replica.Succeed True
          with tags = ["filter", "meta", "run"]
   , testExclude =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                   , parameters = ["--exclude one"]
                                   , testFile = "tests.json"})
          with description = Some "Test tests filtering with \"--exclude\""
          with status = Replica.Succeed True
          with tags = ["filter", "meta", "run"]
   , testTags =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                   , parameters = ["--tags shiny"]
                                   , testFile = "tests.json"})
          with description = Some "Test tests filtering with \"--only\""
          with status = Replica.Succeed True
          with tags = ["filter", "meta", "run"]
   , testExcludeTags =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                   , parameters = ["--exclude-tags shiny"]
                                   , testFile = "tests.json"})
          with description = Some "Test tests filtering with \"--exclude-tags\""
          with status = Replica.Succeed True
          with tags = ["filter", "meta", "run"]
   , testRequire =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/require1"
                                   , testFile = "tests.json"})
          with description = Some "A test failed when its requirements failed"
          with status = Replica.Succeed False
          with tags = ["meta", "require", "run"]

   , testSkipExcludedDependencies =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/require1"
                                   , parameters = ["--only depends_failed"]
                                   , testFile = "tests.json"})
          with description = Some "Dependencies that aren't included in the selected tests are ignored"
          with status = Replica.Succeed True
          with tags = ["meta", "require", "run"]
   , testPunitive =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/allButOne"
                                   , parameters = ["--punitive"]
                                   , testFile = "tests.json"})
          with description = Some "Test the punitive mode"
          with status = Replica.Succeed False
          with tags = ["punitive", "meta", "run"]
   , testPending =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/onePending"
                                   , testFile = "tests.json"})
          with description = Some "Test that pending tests aren't processed"
          with status = Replica.Succeed True
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
   , no_golden_with_inlined_expectation =
       (Meta.replicaTest Meta.Run::{ directory = "tests/replica/inlineMismatch"
                                   , parameters = [ "--interactive" ]
                                   , testFile = "tests.json"})
          with description = Some "No interaction for mismatch when a value is given"
          with require = [ "testOutputMismatch" ]
          with status = Replica.Succeed False
          with tags = ["meta", "run"]
   , ordered_partial_expectation_match = Replica.Success::
       { command = "echo \"Hello, World!\""
       , description = Some "check a partial expectation that succeeds"
       , stdOut = Replica.Consecutive ["Hello", "World"]
       , tags = ["expectation", "run", "partial"]
       }
   , file_expectation1 = Replica.Success::
       { command = "echo \"test\" > tmp123456"
       , description = Some "Check expectation on file"
       , afterTest = ["rm tmp123456"]
       , stdOut = Replica.Generated False
       , files =
         [ { mapKey = "tmp123456"
           , mapValue = Replica.Generated True
           }
         ]
       , tags = ["expectation", "run", "file"]
       }
   , file_expectation2 = Replica.Success::
       { command = "echo \"test\" > tests/tmp123456"
       , description = Some "Check expectation on file in a subdir"
       , afterTest = ["rm tests/tmp123456"]
       , stdOut = Replica.Generated False
       , files =
         [ { mapKey = "tests/tmp123456"
           , mapValue = Replica.Generated True
           }
         ]
       , require = ["file_expectation1"]
       , tags = ["expectation", "run", "file"]
       }
   , error_expectation = Replica.Success::
       { command = "echo \"test\" >&2"
       , description = Some "Check expectation on error"
       , stdOut = Replica.Generated False
       , stdErr = Replica.Generated True
       , tags = ["expectation", "run", "error"]
       }
   , whatever_partial_expectation_match = Replica.Success::
       { command = "echo \"Hello, World!\""
       , description = Some "check a not ordered partial expectation that succeeds"
       , stdOut = Replica.Contains ["World", "Hello"]
       , tags = ["expectation", "run", "partial"]
       }
   , ordered_partial_expectation_mismatch =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/orderedPartialFail"
                                  , testFile = "tests.json"})
           with description = Some "check an ordered partial expectation that fails"
           with status = Replica.Succeed False
           with tags = ["expectation", "run", "partial"]
   , custom_golden_dir =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/goldenDir"
                                  , parameters = ["--golden-dir golden"]
                                  , testFile = "tests.json"})
           with description = Some "test --goldenDir"
           with status = Replica.Succeed True
           with tags = ["config", "golden", "meta"]
   , custom_golden_dir_for_file =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/goldenDirFile"
                                  , parameters = ["--golden-dir golden"]
                                  , testFile = "tests.json"})
           with require = [ "file_expectation1" ]
           with description = Some "test --goldenDir for files"
           with status = Replica.Succeed True
           with tags = ["config", "golden", "meta"]
   , space_unsensitive_ordered_partial_expectation_match = Replica.Success::
       { command = "echo \"Hello, World!\""
       , spaceSensitive = False
       , description = Some "check a space unsensitive partial expectation that succeeds"
       , stdOut = Replica.Consecutive ["Hello ", " World "]
       , tags = ["expectation", "run", "partial", "space"]
       }
   , local_config =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/localConfig"
                                    , testFile = "tests.json"})
           with require = [ "custom_golden_dir" ]
           with description = Some "test localConfig"
           with status = Replica.Succeed True
           with tags = ["config", "local", "meta"]
   , multi_json =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/multi"
                                  , testFile = "tests1.json tests2.json"})
           with description = Some "test several json files"
           with status = Replica.Succeed True
           with tags = ["multi", "meta"]
   , multi_json_error =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/multi"
                                  , testFile = "tests1.json testsDup.json"})
           with description = Some "test several json files"
           with status = Replica.Succeed False
           with tags = ["multi", "meta"]
   , new_json_template =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/new"
                                  , testFile = "generated.json"})
        with description = Some "generated json template should succeed"
        with status = Replica.Succeed True
        with beforeTest = ["${Meta.replica_exe} new generated.json"]
        with afterTest = ["rm -rf .replica", "rm -f generated.json"]
        with tags = ["meta", "new"]
   , new_json_empty_template =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/new"
                                  , testFile = "generated.json"})
        with description = Some "generated empty json template should succeed"
        with status = Replica.Succeed True
        with beforeTest = ["${Meta.replica_exe} new -S generated.json"]
        with afterTest = ["rm -rf .replica", "rm -f generated.json"]
        with tags = ["meta", "new"]
   , new_dhall_template =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/new"
                                  , testFile = "generated.json"})
        with description = Some "generated dhall template should succeed"
        with require = ["new_json_template"]
        with status = Replica.Succeed True
        with beforeTest =
          [ "${Meta.replica_exe} new generated.dhall"
          , "dhall-to-json --file generated.dhall --output generated.json"]
        with afterTest =
          [ "rm -rf .replica", "rm -f generated.json", "rm -f generated.dhall"]
        with tags = ["meta", "new"]
   , new_dhall_empty_template =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/new"
                                  , testFile = "generated.json"})
        with description = Some "generated empty dhall template should succeed"
        with require = ["new_json_empty_template"]
        with status = Replica.Succeed True
        with beforeTest =
          [ "${Meta.replica_exe} new -S generated.dhall"
          , "dhall-to-json --file generated.dhall --output generated.json"]
        with afterTest =
          [ "rm -rf .replica", "rm -f generated.json", "rm -f generated.dhall"]
        with tags = ["meta", "new"]
   }

in tests # parsing_errors # idris
