let Meta = ./Meta/package.dhall

let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:83afdebd588461f9451e17d76551f27e7716774bd1ac610afe33e44b65228b28

let Status = Replica.Status

let tests
    : Replica.Type
    = Replica.Suite
        "parsing"
        ( toMap
            { unknown_command =
                ( Meta.toTest
                    Meta::{ command = "tagada", testFiles = [ "tests.json" ] }
                )
              with description = Some
                  "Unknown commands are rejected, showing help"
              with stdErr = Replica.Expectation.Golden
              with status = Status.Exactly 253
              with tags = [ "meta", "parser" ]
            , unknown_parameter =
                (Meta.Run [ "--oops" ] [ "tests.json" ])
              with description = Some
                  "If a parameter doesn't exist, display an error message and the help"
              with workingDir = Some "tests/replica/two"
              with stdErr = Replica.Expectation.Golden
              with status = Status.Exactly 253
              with tags = [ "meta", "parser" ]
            , opposite_include_exclude =
                (Meta.Run [ "--only one", "--exclude one" ] [ "tests.json" ])
              with description = Some
                  "If a test is both included and rejected, the command fails"
              with workingDir = Some "tests/replica/two"
              with stdErr = Replica.Expectation.Golden
              with status = Status.Exactly 252
              with tags = [ "meta", "parser" ]
            , opposite_include_exclude_tags =
                ( Meta.Run
                    [ "--tags shiny", "--exclude-tags shiny" ]
                    [ "tests.json" ]
                )
              with workingDir = Some "tests/replica/two"
              with description = Some
                  "If a tag is both included and rejected, the command fails"
              with stdErr = Replica.Expectation.Golden
              with status = Status.Exactly 252
              with tags = [ "meta", "parser" ]
            }
        )

in  tests
