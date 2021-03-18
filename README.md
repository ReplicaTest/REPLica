# REPLica

**WIP: file format and command line usage may (often) change.**

Golden tests for Commnd Line interfaces.

## Purpose

Replica aims at managing tests suites composed of command line interfaces calls.

It compares the output of the command line to a "golden value": a stored value of the expected
outcome of the command line call.
If you want a more detailled introduction to golden testing, here is a [nice introduction][golden].

The idea comes from the way tests are implemented in [idris2][idris tests].

## Install

### Requirements

- [idris2](https://idris-lang.org)
- git

### Install

1. Clone this repository
2. Build and install REPLica: `make install`

### Usage

REPLica takes a JSON specification in input.
Though, using [Dhall][] is prefered as it can allow us to build test template
more easily and then translate it to JSON using [dhall-to-json][].

For example, REPLica is tested with itelf, you can check the [test file][] to have an overview of the
possibilities.

You can then explore the tool possibilities with `replica help`.


[Dhall]: https://dhall-lang.org
[idris tests]: https://github.com/idris-lang/Idris2/tree/master/tests
[golden]: https://ro-che.info/articles/2017-12-04-golden-tests
[test file]: https://github.com/berewt/REPLica/blob/main/tests.dhall
[dhall-to-json]: https://github.com/dhall-lang/dhall-haskell/blob/master/dhall-json/README.md
