let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall

let Prelude = Replica.Prelude

let Test = Replica.Test

let Status = Replica.Status

let Expectation = Replica.Expectation

let Meta = ./Meta/package.dhall

let help =
      (Meta.Help ([] : List Text))
      with description = Some "Display a help of all available commands"
      with stdOut = Expectation::{
        , contains =
          [ "replica help"
          , "  run"
          , "  test"
          , "  info"
          , "  set"
          , "  new"
          , " version"
          ]
        }

let helpRun =
      (Meta.Help ([ "run" ] : List Text))
      with description = Some "Help on run command display the right command"
      with stdOut = Expectation::{ contains = [ "replica run" ] }

let helpTest =
      (Meta.Help ([ "test" ] : List Text))
      with description = Some "Help on test command display the right command"
      with stdOut = Expectation::{ contains = [ "replica test", "Alias" ] }

let helpNew =
      (Meta.Help ([ "new" ] : List Text))
      with description = Some "Help on new command display the right command"
      with stdOut = Expectation::{
        , contains = [ "replica new", "dhall", "json" ]
        }

let helpInfo =
      (Meta.Help ([ "info" ] : List Text))
      with description = Some "Display a help of all available info topics"
      with stdOut = Expectation::{
        , contains = [ "replica info", "  test", "  suite" ]
        }

let helpInfoSuite =
      (Meta.Help ([ "info", "suite" ] : List Text))
      with description = Some "Display a help dedicated to the 'suite' topic"
      with stdOut = Expectation::{ contains = [ "replica info suite" ] }

let helpInfoTest =
      (Meta.Help ([ "info", "test" ] : List Text))
      with description = Some "Display a help dedicated to the 'test' topic"
      with stdOut = Expectation::{ contains = [ "replica info test" ] }

let tests
    : Replica.Type
    = Replica.Suite
        "help"
        ( toMap
            { help
            , helpRun
            , helpTest
            , helpNew
            , helpInfo
            , helpInfoSuite
            , helpInfoTest
            }
        )

in  tests
