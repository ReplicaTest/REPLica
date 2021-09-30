let Replica = https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall

let Idris = Replica.Command.Idris
  with default.executable = "${env:PWD as Text}/build/exec/replica"

let Test = Replica.Test
let Status = Replica.Status
let Expectation = Replica.Expectation
let Context = Idris.Context

let tests : Replica.Type = toMap
  { typecheckPackage = (Idris.Typecheck "test.ipkg")
     with workingDir = Some "tests/idris/package"
     with description = Some "Typecheck a package"
     with tags = ["idris", "typecheck"]
  , packageREPL = (Idris.REPL (Context.Package "test.ipkg"))
     with workingDir = Some "tests/idris/package"
     with description = Some "REPL session with a package"
     with input = Some
       ''
       :module Test
       aString
       :q
       ''
     with tags = ["idris", "repl"]
  , fileREPL =
     (Idris.REPL (Context.File {dependencies = [] : List Text, name = "Test.idr"}))
     with workingDir = Some "tests/idris/file"
     with description = Some "REPL session with a file"
     with input = Some
       ''
       aString
       :q
       ''
     with tags = ["idris", "repl"]
  , rawREPL =
     (Idris.REPL (Context.Raw ([] : List Text)))
     with description = Some "REPL session without context"
     with input = Some
       ''
       Just "work!"
       :q
       ''
     with tags = ["idris", "repl"]
  }

in tests
