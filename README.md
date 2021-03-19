# REPLica

Golden tests for Commnd Line interfaces.

## Purpose

Replica aims at managing tests suites composed of command line interfaces calls.

It compares the output of the command line to a "golden value": a stored value of the expected
outcome of the command line call.
If you want a more detailled introduction to golden testing, here is a [nice introduction][golden].

The idea comes from the way tests are implemented in [idris2][idris tests].

Its approach is similar to the one proposed by CI/CD tools like [github actions][] or [gitlab ci][]:
a tests suite is described in a json (or more preferably [dhall][]) file that is processed by the
tool to generate tests.

## Feature

- Test tags
- Check exit status (0 or non 0 only)
- Multi-threads
- Run only selected tests/tags

## Install

Requirements:

- [idris2](https://idris-lang.org)
- [git](https://git-scm.com)
- preferably, [dhall][] and [dhall-to-json][]

Steps:

```shell
# clone repo
git clone git@github.com:berewt/REPLica.git

# install replica
make install

# Ensure that `${HOME}/.local/bin` is in your path

# health-check
replica help
```

## Quickstart

```shell
tee hello.json > /dev/null << EOF
{ "hello": {"command": "echo \"hello, world!\""} }
EOF
```

And then run replica on it: `replica run hello.json`.
You sholu obtain the following result:

```
$ replica run hello.json
------------------------------------------------------------
Running tests...
------------------------------------------------------------
Test results:
  hello: ❌ WrongOutput
------------------------------------------------------------
Summary:
  ❌ (Failure): 1 / 1
```

It's totally fine: `replica` has no golden value for this test yet, we need to build one.
To do so, we will rerun the test in the interactive mode: `replica run --interactive hello.json`.
Now you should be prompted if you want to set the golden value for the test:

```
$ replica run --interactive hello.json
------------------------------------------------------------
Running tests...
hello: Golden value mismatch
Expected: Nothing Found
Given:
hello, world!

Do you want to set the golden value? [N/y]
```

Answer `y` (or `yes`) and the test should pass.
Now that the golden value is set, we can retry to run the suite in a non interactive mode:
`replica run hello.json`...

```
$ replica run hello.json
------------------------------------------------------------
Running tests...
------------------------------------------------------------
Test results:
  hello: ✅
------------------------------------------------------------
Summary:
  ✅ (Success): 1 / 1
```


TADA... it works.

if you want to see it fails again, you can modify the command in `hello.json`.

### Using dhall

REPLica takes a JSON specification in input.
Though, using [dhall][] is prefered as it can allow us to build test templates
and then translate it to JSON using [dhall-to-json][].

For example, the dhall equivalent to the "hello word" example is the following:

```
let Replica = https://raw.githubusercontent.com/berewt/REPLica/main/dhall/replica.dhall

in { hello = Replica.Minimal::{command = "echo \"Hello, world!\""}}
```

Supposed you have a `hello.dhall` file with this content, you can then do:

```
# translate it to json
dhall-to-json hello.dhall -o hello.json
# run the test
replica run hello.json
```

## Going further

REPLica is tested with itelf, you can check the [test file][] to have an overview of the
possibilities.

You can also explore the tool options with `replica help`.


[dhall]: https://dhall-lang.org
[dhall-to-json]: https://github.com/dhall-lang/dhall-haskell/blob/master/dhall-json/README.md
[idris tests]: https://github.com/idris-lang/Idris2/tree/master/tests
[golden]: https://ro-che.info/articles/2017-12-04-golden-tests
[test file]: https://github.com/berewt/REPLica/blob/main/tests.dhall
[gitlab ci]: https://docs.gitlab.com/ee/ci/README.html
[github actions]: https://github.com/features/actions
