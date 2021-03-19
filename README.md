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

### With json

Create a test file:

```shell
tee hello.json > /dev/null << EOF
{ "hello": {"command"='echo "hello,word!"'} }
EOF
```

REPLica takes a JSON specification in input.
Though, using [dhall][] is prefered as it can allow us to build test template
more easily and then translate it to JSON using [dhall-to-json][].

For example, REPLica is tested with itelf, you can check the [test file][] to have an overview of the
possibilities.

You can then explore the tool possibilities with `replica help`.


[dhall]: https://dhall-lang.org
[dhall-to-json]: https://github.com/dhall-lang/dhall-haskell/blob/master/dhall-json/README.md
[idris tests]: https://github.com/idris-lang/Idris2/tree/master/tests
[golden]: https://ro-che.info/articles/2017-12-04-golden-tests
[test file]: https://github.com/berewt/REPLica/blob/main/tests.dhall
