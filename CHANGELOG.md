# REPLica changelog

## Since version 0.4.0

- CHANGE:
    * The dhall language for replica has changed and is now hosted in its own project.
      If you have existing `dhall` tests that you don't want to change, you can use this
      url as an entry point for replica:
      https://raw.githubusercontent.com/ReplicaTest/REPLica/v0.4.0/dhall/replica.dhall
    * Migration to Idris2 0.5.1
- NEW:
    * `replica new` to create tests file templates
    * `suite` to organise your test by suites
    * test name with spaces are officially supported
    * execution time of each test is tracked
- FIX:
    * `replica help` mentions `version`
    * skipped tests are displayed

## version 0.4.0

- CHANGE:
    * Expectation format: Note if you use "partial expectations" in your tests, you need to upgrade
      your tests suites.
- NEW:
    * New expectations functions: `contains`, `consecutive`, `start`, `end`
    * Differentiate `stdOut` and `stdErr`
    * command exit code can be checked
    * we can submit several json files
    * add a `version` subcommand

## version 0.3.1

- FIX documentation error in replica set
- CHANGE docker entry point is now a shell

## version 0.3.0

- Can mark tests as _pending_
- Can set `expected` in json and dhall to specify expected output directly
- Can check the content of a generated file using `outputFile`
- Partial expectations
- Space sensitivity is configurable
- Directory for golden values is configurable
- external configuration (via `replica set`)
- nix flake (thanks to Matthieu Coudron)
- dockerfile

## version 0.2.0

- Tests suites are in json format, with dhall support
- Tests have:
    * tags
    * dependencies
    * description
    * pre/post acitions
    * definition of a working directories
    * input that will replace stdin
- Multi-threading is supported (tests are sent by bathches of n threads)
- Expcetations, last outputs, and tests results are stored in `.replica`
- Filters to run a subset of tests

## version 0.1

- direct port of idris2 testing libraries, with a configuration file _à la_ idris package.
