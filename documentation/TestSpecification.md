# Test Specification

REPLica tests suites are specified in a JSON file.
Although, it is probably more convenient to use [Dhall][] and [`dhall-to-json`][] to benefit
of a typespace and more concise way to write them.

## Test suite

A test suite is a JSON object (or a Dhall record) in which each field must be a test.

## Test definition

### Available fields

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
| `spaceSensitive` | Boolean | Bool | No | `True` | If set, the spaces are normalized before comparing the given and expected output: each chunk of space-like character are replaced by a single space and empty-lines are not considered |
| `expectation` | String / Object | Optional Expectation | No | | See [Expectation](#Expectation) |
| `outputFile` | String | Optional Text | No | | If set. REPLica will compare the content of the given file to a golden value. |

The default value are infered in JSON.
In Dhall, you need to use `Replica.Minimal` a schema that populate a test record with the default
values.

Aside `Minimal`, Dhall provides two other schema `Success` and `Failure`, that respectively
set the `succeed` value to `Some True` and `Some False`.

## Expectation

By default the behaviour of REPLica is to wait for a golden value to be saved
(generaly thanks to `replica run --interactive`) and then to compare the output of the
next runs with this _golden value_.

However, users may wants to inline their own expectations directly in the test.
This can be done by setting the `expectation` field.

If the field is set, the given value is _immutable_,
it cannot be changed using the interactive mode.

### JSON

There is two types of values that are supported for expactations:

- **Strings.** The given string define the exact value that must be match by the output of the
  command (after a potential normalisation of the space if `spaceSensitive` is set to `False`).
- **An array of strings.** It defines a partial expectation: the result of the command must contain
  each member of the array. If `spaceSensitive` is set to false, both the output and the
  expectations are normalized before comparison.
- **An object.** Two fields are then considered:
    - `parts`: it must be an array of strings, and define the partial matches (mandatory).
    - `ordered`: a boolean that indicates if the different strings in parts should be ordered or
      not (optional, default is not ordered).

### Dhall

The corresponding specification in dhall is the following:
the type of `expectation` is `Optional Expectation`,
where `Expectation `is a sum type, defined as follows:

```
let Expectation
    : Type
    = < PartialExp : PartialExpectation | ExactExp : Text >
```

with this definition for `PartialExpectation`:

```
let PartialExpectation
    : Type
    = { ordered : Bool
      , parts : List Text
      }
```

Reader may refer to the [JSON](#JSON) for the semantic of the fields.

As using sum types out of the box is a bit verbose in dhall, some smart constructors are provided
to ease their use:

- `Partial : Bool -> List Text -> Expectation` allow you to build an `Expectation.PartialExp`.
- `Exact : Text -> Expectation` allow you to build an `Expectation.ExactExp`.

## How test are executed

See the [Test execution][] section to learn more about how REPLica handles a specification.

[Dhall]: https://dhall-lang.org/
[`dhall-to-json`]: https://github.com/dhall-lang/dhall-haskell/blob/master/dhall-json/README.md
[Test execution]: ./TestExecution.md
