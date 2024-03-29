let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:e89a5d8a50bf5551f1012d7c627ab6d1fd278148a7341682247b2e024fcf90d4

let Idris =
      Replica.Command.Idris
      with default.executable = "${env:PWD as Text}/build/exec/replica"

let Context = Idris.Context

let tests
    : Replica.Type
    = Replica.Suite
        "idris"
        ( toMap
            { typecheckPackage =
                (Idris.Typecheck "test.ipkg")
              with workingDir = Some "tests/idris/package"
              with description = Some "Typecheck a package"
              with tags = [ "idris", "typecheck" ]
            , packageREPL =
                (Idris.REPL (Context.Package "test.ipkg"))
              with workingDir = Some "tests/idris/package"
              with description = Some "REPL session with a package"
              with input = Some
                  ''
                  :module Test
                  aString
                  :q
                  ''
              with tags = [ "idris", "repl" ]
            , fileREPL =
                ( Idris.REPL
                    ( Context.File
                        { dependencies = [] : List Text, name = "Test.idr" }
                    )
                )
              with workingDir = Some "tests/idris/file"
              with description = Some "REPL session with a file"
              with input = Some
                  ''
                  aString
                  :q
                  ''
              with tags = [ "idris", "repl" ]
            , rawREPL =
                (Idris.REPL (Context.Raw ([] : List Text)))
              with description = Some "REPL session without context"
              with input = Some
                  ''
                  Just "work!"
                  :q
                  ''
              with tags = [ "idris", "repl" ]
            }
        )

in  tests
