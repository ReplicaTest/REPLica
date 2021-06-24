let Replica = ../dhall/replica.dhall
let Meta = ./meta.dhall

let tests : Replica.Replica = [
  { mapKey = "unknown_command"
  , mapValue =
      (Meta.replicaTest { command = "tagada"
                        , directory = "tests/replica/two"
                        , parameters = [] : List Text
                        , testFile = "tests.json"
                        })
      with description = Some "Unknown commands are rejected, showing help"
      with status = Replica.Exactly 254
      with tags = ["meta","parser"]
  },
  { mapKey = "unknown_parameter"
  , mapValue =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                  , parameters = ["--oops"]
                                  , testFile = "tests.json"
                                  })
      with description = Some "If a parameter doesn't exist, display an error message and the help"
      with tags = ["meta","parser"]
  },
  { mapKey = "opposite_include_exclude"
  , mapValue =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                  , parameters = ["--only one", "--exclude one"]
                                  , testFile = "tests.json"
                                  })
      with description = Some "If a test is both included and rejected, the command fails"
      with tags = ["meta","parser"]
  },
  { mapKey = "opposite_include_exclude_tags"
  , mapValue =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                  , parameters = ["--tags shiny", "--exclude-tags shiny"]
                                  , testFile = "tests.json"
                                  })
      with description = Some "If a tag is both included and rejected, the command fails"
      with tags = ["meta","parser"]
  }
  ]

in tests
