# Test Specification

REPLica tests suites are specified in a JSON file.
Although, it is probably more convenient to use
[Dhall][] and [`dhall-to-json`][] to benefit
of a typespace and more concise way to write them.

## Test suite

A test suite is a JSON object (or a Dhall record) in which each field must be a test.

## Test definition

### Available fields

A test is a JSON object or a Dhall record.
Here is the list of available fields:

<!-- markdownlint-disable MD013 MD033 -->
| Field name | JSON Type | Dhall Type | Mandatory | Default | Description |
| :--------- | --------- | ---------- | :-------: | ------- | ----------- |
| `description` | String | Optional Text |  | Use when you display text info |
| `command`  | String | Text | Yes | | The tested command. |
| `workingDir` | String | Optional Text | No | `.` |  The directory where the test is executed |
| `beforeTest` | Array String | List Text | No | `[]` | A list of command to execute before the test. <br /> It is ran in a separated shell and thus you can't declare environment variables needed for the test here. |
| `afterTest` | Array String | List Text | No | `[]` | A list of command to execute after the test. <br /> It is ran in a separated shell and thus you can't access the environment variables declared in the test here. |
| `input` | String | Optional Text | No | | Text that is sent to the test command as standard input |
| `require` | Array String | List Text | No | `[]` | A list of tests that must succeed before this one can be triggered
| `tags` | Array String | List Text | No | `[]` | Used to classify tests |
| `pending` | Boolean | Bool | No | `False` | Pending tests won't be executed |
| `succeed` | Boolean | Optional Bool | No | | If set, REPLica will check the value returned by the command |
| `spaceSensitive` | Boolean | Bool | No | `True` | If set, the spaces are normalized before comparing the given and expected output: each chunk of space-like character are replaced by a single space and empty-lines are not considered |
| `stdOut` | Anything but an integer | Optional Expectation | No | True | set the expectation for `stdOut`, see [Expectation](#expectation) |
| `stdErr` | Anything but an integer | Optional Expectation | No | False | set the expectation for `stdErr`, see [Expectation](#expectation) |
| `files` | Object | Map Text Expectation | No | | List the files to check, and set the
corresponding expectation, see [Expectation](#expectation) |
<!-- markdownlint-enable MD013 MD033 -->

The default value are infered in JSON.
In Dhall, you need to use `Replica.Test` a schema that populate
a test record with the default values.

Aside `Test`, Dhall provides two other schema `Replica.Test.Success`
and `Replicat.Test.Failure`,
which respectively set the `succeed` value to `Some True` and `Some False`.

## Expectation

By default the behaviour of REPLica is to wait for a golden value
for `stdOut` to be saved
(generaly thanks to `replica run --interactive`)
and then to compare the output of the next runs with this _golden value_.

However, users may wants to inline their own expectations directly in the test.
This can be done by setting the `stdOut`, `stdErr` and `file` fields.

The semantic of an `expectation` depends on the type of its value.

### JSON

There is three types of values that are supported for expactations:

- **Booleans.** If true, use a golden value.
  If false, explicitly skip this source.
- **null.** don't check this source (equivalent to `false`)
- **Strings.** The given string define the exact value that
  must be match by the output of the command
  (after a potential normalisation of the space
  if `spaceSensitive` is set to `False`).
- **An array of strings.** It defines a partial expectation:
  the result of the command must contain each member of the array.
  If `spaceSensitive` is set to false, both the output and the
  expectations are normalized before comparison.
- **An object**: allow the defitinion of several requirements that must all be satisfied.
  The recognised fields are:

  - `generated`: A boolean that indicates whether or not we use a golden value
  - `exact`: Check for that exact string
  - `start`: Check if the source starts with this string
  - `end`: Check if the source ends with this string
  - `contains`: Check if the source contains all the strings of the provided
    list of strings
  - `consecutive`: Check if the source contains all the strings
    of the provided list of strings, in the given order.

### Dhall

The corresponding specification in dhall is the following:

```dhall
let Replica.Expectation
    : Type
    = { generated : Bool
      , exact : Optional Text
      , start : Optional Text
      , end: Optional Text
      , consecutive : List Text
      , contains : List Text
      }
```

Reader may refer to the [JSON section](#json) for the semantic of the fields.

A few helper are available, to ease the definition of usual expectations:
to ease their use:

- `Golden : Expectation` allows you to use a golden value for the given entry.
- `Ignored : Expectation` ignore this output.
- `Exact : Text -> Expectation` allows you to check that the entry is exactly
  the given string.
- `Contains : List Text -> Expectation` check that each value is in the output.
- `Consecutive : List Text -> Expectation` check that each value is present
  in the output, in this
  order (possibly separated by other parts of text)
  of consecutive values to check.

## How test are executed

See the [Test execution][] section to learn more about how REPLica handles a specification.

[Dhall]: https://dhall-lang.org/
[`dhall-to-json`]: https://github.com/dhall-lang/dhall-haskell/blob/master/dhall-json/README.md
[Test execution]: ./TestExecution.md
