let Meta = ./tests/Meta/package.dhall

let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:83afdebd588461f9451e17d76551f27e7716774bd1ac610afe33e44b65228b28

let Test = Replica.Test

let Status = Replica.Status

let Expectation = Replica.Expectation

let runTestsJSON = Meta.Run ([] : List Text) [ "tests.json" ]

let basicTests
    : Replica.Type
    = Replica.Suite
        "basic"
        ( toMap
            { simplest_success = Test::{
              , command = "true"
              , description = Some
                  "A call to 'true' must succeed with no output"
              }
            , test_success = Test.Success::{
              , command = "true"
              , description = Some "Check the success exit code"
              }
            , test_failure = Test.Failure::{
              , command = "false"
              , description = Some "Test a non null exit code"
              }
            }
        )

let templateTests
    : Replica.Type
    = Replica.Suite
        "template"
        ( toMap
            { new_json_template =
                (Meta.Run ([] : List Text) [ "generated.json" ])
              with workingDir = Some "tests/replica/new"
              with description = Some "generated json template should succeed"
              with status = Status.Success
              with beforeTest =
                [ "${Meta.default.executable} new generated.json" ]
              with afterTest = [ "rm -rf .replica", "rm -f generated.json" ]
              with tags = [ "meta" ]
            , new_json_empty_template =
                (Meta.Run ([] : List Text) [ "generated.json" ])
              with workingDir = Some "tests/replica/new"
              with description = Some
                  "generated empty json template should succeed"
              with status = Status.Success
              with beforeTest =
                [ "${Meta.default.executable} new -S generated.json" ]
              with afterTest = [ "rm -rf .replica", "rm -f generated.json" ]
              with tags = [ "meta" ]
            , new_dhall_template =
                (Meta.Run ([] : List Text) [ "generated.json" ])
              with workingDir = Some "tests/replica/new"
              with description = Some "generated dhall template should succeed"
              with require = [ "new_json_template" ]
              with status = Status.Success
              with beforeTest =
                [ "${Meta.default.executable} new generated.dhall"
                , "dhall-to-json --file generated.dhall --output generated.json"
                ]
              with afterTest =
                [ "rm -rf .replica"
                , "rm -f generated.json"
                , "rm -f generated.dhall"
                ]
              with tags = [ "meta", "online" ]
            , new_dhall_empty_template =
                (Meta.Run ([] : List Text) [ "generated.json" ])
              with workingDir = Some "tests/replica/new"
              with description = Some
                  "generated empty dhall template should succeed"
              with require = [ "new_json_empty_template" ]
              with status = Status.Success
              with beforeTest =
                [ "${Meta.default.executable} new -S generated.dhall"
                , "dhall-to-json --file generated.dhall --output generated.json"
                ]
              with afterTest =
                [ "rm -rf .replica"
                , "rm -f generated.json"
                , "rm -f generated.dhall"
                ]
              with tags = [ "meta", "online" ]
            }
        )

let filterTests
    : Replica.Type
    = Replica.Suite
        "filter"
        ( toMap
            { testOnly =
                (Meta.Run [ "--only one" ] [ "tests.json" ])
              with workingDir = Some "tests/replica/two"
              with description = Some "Test tests filtering with \"--only\""
              with status = Status.Success
              with tags = [ "filter", "meta", "run" ]
            , testExclude =
                (Meta.Run [ "--exclude one" ] [ "tests.json" ])
              with workingDir = Some "./tests/replica/two"
              with description = Some "Test tests filtering with \"--exclude\""
              with status = Status.Success
              with tags = [ "filter", "meta", "run" ]
            , testTags =
                (Meta.Run [ "--tags shiny" ] [ "tests.json" ])
              with workingDir = Some "./tests/replica/two"
              with description = Some "Test tests filtering with \"--tags\""
              with status = Status.Success
              with tags = [ "filter", "meta", "run" ]
            , testExcludeTags =
                (Meta.Run [ "--exclude-tags shiny" ] [ "tests.json" ])
              with workingDir = Some "./tests/replica/two"
              with description = Some
                  "Test tests filtering with \"--exclude-tags\""
              with status = Status.Success
              with tags = [ "filter", "meta", "run" ]
            , testSkipExcludedDependencies =
                (Meta.Run [ "--only depends_failed" ] [ "tests.json" ])
              with workingDir = Some "tests/replica/require1"
              with description = Some
                  "Dependencies that aren't included in the selected tests are ignored"
              with status = Status.Success
              with tags = [ "meta", "require", "run" ]
            }
        )

let skipExecutionTests
    : Replica.Type
    = Replica.Suite
        "skip"
        ( toMap
            { testRequire =
                runTestsJSON
              with workingDir = Some "tests/replica/require1"
              with description = Some
                  "A test failed when its requirements failed"
              with status = Status.Failure
              with tags = [ "meta", "require", "run" ]
            , testPunitive =
                (Meta.Run [ "--punitive" ] [ "tests.json" ])
              with workingDir = Some "tests/replica/allButOne"
              with description = Some "Test the punitive mode"
              with status = Status.Failure
              with tags = [ "punitive", "meta", "run" ]
            , testPending =
                runTestsJSON
              with workingDir = Some "tests/replica/onePending"
              with description = Some "Test that pending tests aren't processed"
              with status = Status.Success
              with tags = [ "pending", "meta", "run" ]
            }
        )

let configTests
    : Replica.Type
    = Replica.Suite
        "config"
        ( toMap
            { custom_golden_dir =
                (Meta.Run [ "--golden-dir golden" ] [ "tests.json" ])
              with workingDir = Some "tests/replica/goldenDir"
              with description = Some "test --goldenDir"
              with status = Status.Success
              with tags = [ "golden", "meta" ]
            , custom_golden_dir_for_file =
                (Meta.Run [ "--golden-dir golden" ] [ "tests.json" ])
              with workingDir = Some "tests/replica/goldenDirFile"
              with require = [ "file_expectation1" ]
              with description = Some "test --goldenDir for files"
              with status = Status.Success
              with tags = [ "golden", "meta" ]
            , local_config =
                runTestsJSON
              with workingDir = Some "tests/replica/localConfig"
              with require = [ "custom_golden_dir" ]
              with description = Some "test localConfig"
              with status = Status.Success
              with tags = [ "local", "meta" ]
            }
        )

let expectationsTests
    : Replica.Type
    = Replica.Suite
        "expectations"
        ( toMap
            { test_given_expectation = Test.Success::{
              , command = "echo \"Hello, world!\""
              , description = Some
                  "We use the given expectation field if it exists"
              , stdOut =
                  Expectation.Exact
                    ''
                    Hello, world!
                    ''
              }
            , no_golden_with_inlined_expectation =
                (Meta.Run [ "--interactive" ] [ "tests.json" ])
              with workingDir = Some "tests/replica/inlineMismatch"
              with description = Some
                  "No interaction for mismatch when a value is given"
              with require = [ "testOutputMismatch" ]
              with status = Status.Failure
              with tags = [ "meta", "run" ]
            , start_success = Test.Success::{
              , command = "echo \"Hello, World!\""
              , description = Some "check a start expectation that succeeds"
              , stdOut = Expectation::{ start = Some "Hello" }
              , tags = [ "run", "partial", "start" ]
              }
            , start_fail =
                runTestsJSON
              with workingDir = Some "tests/replica/start_fail"
              with description = Some "Check a start expectation that fails"
              with status = Status.Failure
              with tags = [ "run", "partial", "start" ]
            , end_success = Test.Success::{
              , command = "echo \"Hello, World!\""
              , description = Some "Check an end expectation that succeeds"
              , stdOut = Expectation::{
                , end = Some
                    ''
                    World!
                    ''
                }
              , tags = [ "run", "partial", "end" ]
              }
            , end_fail =
                runTestsJSON
              with workingDir = Some "tests/replica/end_fail"
              with description = Some "Check an end expectation that fails"
              with status = Status.Failure
              with tags = [ "run", "partial", "end" ]
            , ordered_partial_expectation_match = Test.Success::{
              , command = "echo \"Hello, World!\""
              , description = Some "check a partial expectation that succeeds"
              , stdOut = Expectation.Consecutive [ "Hello", "World" ]
              , tags = [ "run", "partial" ]
              }
            , space_unsensitive_ordered_partial_expectation_match = Test.Success::{
              , command = "echo \"Hello, World!\""
              , spaceSensitive = False
              , description = Some
                  "check a space unsensitive partial expectation that succeeds"
              , stdOut = Expectation.Consecutive [ "Hello ", " World " ]
              , tags = [ "run", "partial", "space" ]
              }
            , whatever_partial_expectation_match = Test.Success::{
              , command = "echo \"Hello, World!\""
              , description = Some
                  "check a not ordered partial expectation that succeeds"
              , stdOut = Expectation.Contains [ "World", "Hello" ]
              , tags = [ "run", "partial" ]
              }
            , ordered_partial_expectation_mismatch =
                runTestsJSON
              with workingDir = Some "tests/replica/orderedPartialFail"
              with description = Some
                  "check an ordered partial expectation that fails"
              with status = Status.Failure
              with tags = [ "expectation", "run", "partial" ]
            , file_expectation1 = Test.Success::{
              , command = "echo \"test\" > tmp123456"
              , description = Some "Check expectation on file"
              , afterTest = [ "rm tmp123456" ]
              , stdOut = Expectation.Ignored
              , files = toMap { tmp123456 = Expectation.Golden }
              , tags = [ "run", "file" ]
              }
            , file_expectation2 = Test.Success::{
              , command = "echo \"test\" > tests/tmp123456"
              , description = Some "Check expectation on file in a subdir"
              , afterTest = [ "rm tests/tmp123456" ]
              , stdOut = Expectation.Ignored
              , files = toMap { tests/tmp123456 = Expectation.Golden }
              , require = [ "file_expectation1" ]
              , tags = [ "run", "file" ]
              }
            , error_expectation = Test.Success::{
              , command = "echo \"test\" >&2"
              , description = Some "Check expectation on error"
              , stdOut = Expectation.Ignored
              , stdErr = Expectation.Golden
              , tags = [ "run", "error" ]
              }
            }
        )

let tests
    : Replica.Type
    =   basicTests
      # templateTests
      # filterTests
      # skipExecutionTests
      # expectationsTests
      # configTests

in    tests
    # ./tests/parsing_errors.dhall
    # ./tests/help.dhall
    # ./tests/suite.dhall
    # ./tests/idris.dhall
    # [ { mapKey = "test space"
        , mapValue = Test::{
          , command = "echo \"Hello, World!\""
          , description = Some "Test space in test name"
          }
        }
      ]
    # toMap
        { testWorkingDir = Test.Success::{
          , command = "./run.sh"
          , description = Some
              "Test that the workingDir parameter is taken into account"
          , workingDir = Some "tests/basic"
          }
        , testOutput = Test.Success::{
          , command = "echo \"Hello, World!\""
          , description = Some "Output is checked correctly"
          }
        , testBefore = Test.Success::{
          , command = "cat new.txt"
          , description = Some "Test that beforeTest is executed correctly"
          , workingDir = Some "tests/basic"
          , beforeTest = [ "echo \"Fresh content\" > new.txt" ]
          , afterTest = [ "rm new.txt" ]
          , tags = [ "beforeTest" ]
          }
        , testReplica =
            runTestsJSON
          with workingDir = Some "tests/replica/empty"
          with description = Some "Test that an empty test suite is passing"
          with status = Status.Success
          with tags = [ "meta", "run" ]
        , testOutputMismatch =
            runTestsJSON
          with workingDir = Some "tests/replica/mismatch"
          with description = Some "Content mismatch is printed on error"
          with status = Status.Failure
          with tags = [ "meta", "run" ]
        , testBeforeTestFailImpact =
            runTestsJSON
          with workingDir = Some "tests/replica/beforeFailed"
          with description = Some
              "we should be able to fallback to a normal behaviour after a failure of beforeTest"
          with tags = [ "beforeTest" ]
        , testInput = Test.Success::{
          , command = "cat"
          , input = Some "hello, world"
          , description = Some "pass input to the command"
          }
        , too_many_error =
            runTestsJSON
          with workingDir = Some "tests/replica/tooManyError"
          with description = Some "test error code with too many error"
          with status = Status.Exactly 128
          with stdOut = Expectation.Ignored
          with tags = [ "run", "error", "meta" ]
        , multi_json =
            (Meta.Run ([] : List Text) [ "tests1.json", "tests2.json" ])
          with workingDir = Some "tests/replica/multi"
          with description = Some "test several json files"
          with status = Status.Success
          with tags = [ "multi", "meta" ]
        , multi_json_error =
            (Meta.Run ([] : List Text) [ "tests1.json", "testsDup.json" ])
          with workingDir = Some "tests/replica/multi"
          with description = Some "test several json files"
          with status = Status.Failure
          with stdErr = Replica.Expectation.Golden
          with tags = [ "multi", "meta" ]
        }
