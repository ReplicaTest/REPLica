let Replica = https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall

let Meta = ./Meta/package.dhall

let Test = Replica.Test
let Status = Replica.Status
let Expectation = Replica.Expectation

let runTestsJSON = Meta.Run ([] : List Text) ["tests.json"]

let tests : Replica.Type = toMap
  { unknown_command = (Meta.toTest (Meta ::
      { command = "tagada"
      , testFiles = ["tests.json"]
      }))
      with description = Some "Unknown commands are rejected, showing help"
      with status = Status.Exactly 254
      with tags = ["meta","parser"]
  , unknown_parameter = (Meta.Run ["--oops"] ["tests.json"])
      with description = Some "If a parameter doesn't exist, display an error message and the help"
      with workingDir = Some "tests/replica/two"
      with tags = ["meta","parser"]
  , opposite_include_exclude = (Meta.Run ["--only one", "--exclude one"] ["tests.json"])
      with workingDir = Some "tests/replica/two"
      with description = Some "If a test is both included and rejected, the command fails"
      with tags = ["meta","parser"]
  , opposite_include_exclude_tags =
      (Meta.Run ["--tags shiny", "--exclude-tags shiny"] ["tests.json"])
      with workingDir = Some "tests/replica/two"
      with description = Some "If a tag is both included and rejected, the command fails"
      with tags = ["meta","parser"]
  }

in tests
