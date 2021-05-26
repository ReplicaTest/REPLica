# REPLica

[![ci](https://github.com/berewt/REPLica/actions/workflows/ci.yml/badge.svg)](https://github.com/berewt/REPLica/actions/workflows/ci.yml)

Golden tests for Command Line interfaces.

## Purpose

Replica aims at managing tests suites composed of command line interfaces calls.

It compares the output of the command line to a "golden value": a stored value of the expected
outcome of the command line call.
If you want a more detailed introduction to golden testing, here is a [nice introduction][golden].

The idea comes from the way tests are implemented in [idris2][idris tests].

Its approach is similar to the one proposed by CI/CD tools like [github actions][] or [gitlab ci][]:
a tests suite is described in a json (or more preferably [dhall][]) file that is processed by the
tool to generate tests.

### Why REPLica?

There are few frameworks that are dedicated to CLI tests.
None of them, to my knowledge, mix of a structured document to document the test
and of interactive golden value generation to specify the expectation.

This approach ease the modification of the CLI output in the early development phases and
provide a clear syntax for test development and maintenance.

Other CLI testing frameworks

- [Idris2 test package][idris tests]: REPLica's daddy. REPLica was created with the idea to provide
  a more structured way to write tests (the JSON/Dhall specification) and to develop the
  functionalities (see the [features])
- [Pester](https://github.com/pester/Pester): By far more mature than REPLica, thought for
  powershell, it includes test coverage, test discovery, complex expectations DSL and so on.
  Pester doesn't, however, provides a way to generate expectation from previous run.
- [shelltestrunner](https://hackage.haskell.org/package/shelltestrunner): another minimal tool to
  test CLI, but without golden value generation or test tags.

### Features

- Test tags
- Test dependencies (test are run only if other tests succeed)
- Check exit status (0 or non 0 only)
- Multi-threads
- Run only selected tests/tags
- Partial expectation
- Test output file if any

## Install

### Requirements

- [idris2](https://idris-lang.org)
- [git](https://git-scm.com)
- preferably, [dhall][] and [dhall-to-json][]

**Regarding idris2 version:**

As idris is in active development,
REPLica try to stay compatible with the latest snapshots of idris2.

A consequence is that, unfortunately, REPLica is not compatible with idris2 0.3.0, which
is usually provided by `brew` and `nix`.

The plan in the future is to stabilise the code on the next major idris2 release.

You must either build `idris2` [from source][idris_repo] or use [docker][idris_docker].

### Steps

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
dhall-to-json --file hello.dhall -o hello.json
# run the test
replica run hello.json
```

## Going further

REPLica is tested with itelf, you can check the [test file][] to have an overview of the
possibilities.

The [documentation](./documentation) folder also contain useful pieces of information:

- The [tests specification](./documentation/TestSpecification.md) in JSON and Dhall.
- A description of the [tests execution workplan](./documentation/TestExecution.md).

You can also explore the tool options with `replica help`.

### replica in my project?

The [utils](./utils) folder contains a few helpers to ease the integration of
replica in `git` and `Make`.

## Roadmap

I keep track of the things I want to implement in a
[dedicated project](https://github.com/berewt/REPLica/projects/3).
If you think that something is missing, don't hesitate to submit a feature request.


## Help and support

PR are welcome.
If you use the tool, I'd be happy to know about it, drop me a line on
[twitter](https://twitter.com/berewt).

[dhall]: https://dhall-lang.org
[dhall-to-json]: https://github.com/dhall-lang/dhall-haskell/blob/master/dhall-json/README.md
[idris tests]: https://github.com/idris-lang/Idris2/tree/master/tests
[idris_repo]: https://github.com/idris-lang/Idris2
[idris_docker]: https://hub.docker.com/r/snazzybucket/idris2
[golden]: https://ro-che.info/articles/2017-12-04-golden-tests
[test file]: https://github.com/berewt/REPLica/blob/main/tests.dhall
[gitlab ci]: https://docs.gitlab.com/ee/ci/README.html
[github actions]: https://github.com/features/actions
