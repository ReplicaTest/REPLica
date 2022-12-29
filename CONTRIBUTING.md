# Contributing to REPLica

## Setting up an environment

The best way to setup a working environment is to use [`nix flakes`][nix-flakes]

`nix develop` should bring everything you need:
the current `idris2` version we use and its libraries,
`dhall` and `dhall-to-json`.

[nix-flakes]: https://nixos.wiki/wiki/Flakes

## Running the tests

`make test` is running all the tests referenced in `tests.dhall`.
Note that the tests are run by the current version of `REPLica`, so if you're
changing the behaviour of `REPLica` during your development, it may impact the
tests.
If you prefer to use a stable version of replica to launch your tests, you can
set the `REPLICA_EXE` variable to the path of the replica version you want to
use.

In the end though, the tests must pass with the current version.

If you want to update the golden values of the tests, you can use `make generate`.

All contributions are welcome.
