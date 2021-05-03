# Test Specification

REPLica tests suites are specified in a JSON file.
Although, it is probably more convenient to use [Dhall][] and [`dhall-to-json`][] to benefit
of a typespace and more concise way to write them.

## Test definition

# Available field

A test is a JSON object or a Dhall record.
Here is the list of available fields:

| Field name | JSON Type | Dhall Type | Mandatory | Default | Description |
| :--------- | --------- | ---------- | :-------: | ------- | ----------- |
| `description` | String | Optional Text |  | Use when you display text info |
| `command`  | String | Text | Yes | | The tested command. |
| `workingDir` | String | Optional Text | No | `.` |  The directory where the test is executed |
| `beforeTest` | Array String | List Text | No | `[]` | A list of command to execute before the test. <br /> It is ran in a separated shell and thus you can't declare environment variables needed for the test here, for example. |
| `afterTest` | Array String | List Text | No | `[]` | A list of command to execute after the test. <br /> It is ran in a separated shell and thus you can't access the environment variables declared in the test here. |
| `input` | String | Optional Text | No | | Text that is sent to the test command as standard input |
| `require` | Array String | List Text | No | `[]` | A list of tests that must succeed before this one can be triggered
| `tags` | Array String | List Text | No | `[]` | Used to classify tests |
| `pending` | Boolean | Bool | No | `False` | Pending tests won't be executed |
| `succeed` | Boolean | Optional Bool | No | | If set, REPLica will check the value returned by the command |
| `expectation` | String | Optional Text | No | If you don't want to generate golden values interactively, you can specify the expected output with this field. |
| `outputFile` | String | Optional Text | No | | If set. REPLica will compare the content of the given file to a golden value. |

The default value are infered in JSON.
In Dhall, you need to use `Replica.Minimal` a schema that populate a test record with the default
values.

Aside `Minimal`, Dhall provides two other schema `Success` and `Failure`, that respectively
set the `succeed` value to `Some True` and `Some False`.

## Test suite

A test suite is a JSON object (or a Dhall record) in which each field must be a test.

## How test are executed

See the [Test execution][] section to learn more about how REPLica handles a specification.

[Dhall]: https://dhall-lang.org/
[`dhall-to-json`]: https://github.com/dhall-lang/dhall-haskell/blob/master/dhall-json/README.md
[Test execution]: ./TestExecution.md
