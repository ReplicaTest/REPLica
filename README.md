# REPLica

**WIP: file format and command line usage may change often.**

Test things command line commands.

## Purpose

Replica aims at managing tests suites composed of command line itnerfaces calls.

It compares the output of the command line to a "golden number": a stored value of the expected
outcome of the command line call.

The idea comes from the way tests are implemented in [idris2][idris tests].

## Install

### Requirements

- [idris2](https://idris-lang.org)
- git

### Install

1. Clone this repository
2. Build and install REPLica: `make install`

## (Current) Usage

Once you have a directory with a [replica test](#test-structure), just run:

`replica <test path>`

## Test structure

A test directory should contain:

- a `test.repl`, which describes the command the test;
- a `expected` file, with the expected output of the command.

# test.repl

Here is an example:

```
test testName -- mandatory preamble (though, testnName is not used at the moment)

exec = idris2 -- the nmae of the command to run (madatory)

params = "--repl mypackage.ipkg" -- optional parameters to send to the command line

input = "myInput.txt" -- the path of a file that contains the input to be sent to the command line

output = "output" -- the name of the output file that will store the result of the execution
```

This example will lead to the execution of the following command:

`idris 2 --repl mypackage.ipkg < myInpu.txtt > output`

And then compare it to the `expected` file of this directory.

## Example

REPLica is tested with REPLica, you can have a look at the [test directory](tests)

## Roadmap

- Automatic generation of golden numbers
- Custom tests descrriptors
- Test suite management (through a tag system)
- Command mapping (options to change the name of the command to test)
- Interactive mode when test failed
- Cleaner file format

[idris tests]: https://github.com/idris-lang/Idris2/tree/master/tests
