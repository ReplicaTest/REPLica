# REPLica changelog

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
