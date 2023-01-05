# Test execution workflow

When you launch a `replica run` command, the following steps happen:

1. The tool list tests that can be direcly executed
   (those that don't have any test in their `require` fields).
2. For each of these tests, here are the steps:
    0. The test is declared as `pending`, we skip it and pass to the next one.
    1. Move to the directory defined in `workingDir`, if any.
    2. Run the `beforeTest`, if any and collect the output.
    3. Run the `afterTest`.
    4. Check the exit status of the tested command. If `mustSucceed` is set
       and the exit status is not valid, me stop here and the test failed.
       Otherwise, we continue.
    5. Retrieve the expected content for the standard output
       and if `outputFile` is defined, for the generated file.
    6. Compare the result of the command with the expectations, either:
         * We got the expected result and the test succeeded.
         * There's a mismatch and we continue.
    7. If `replica` is run in interactive mode,
       the difference between the expected values
       and the given values are displayed,
       and the user can set/replace the values for the test.
3. The tests that didn't explicitly failed are removed of the `require` field
   of the remaining tests.
4. If at least one tests succeeded and there is at least one test left,
   we loop.
   Otherwise, we show the results.
