let Replica = ../dhall/replica.dhall
let Meta = ../dhall/meta.dhall

let tests : Replica.Replica = [
  { mapKey = "unknown_command"
  , mapValue =
      (Meta.replicaTest { command = "tagada"
                        , directory = "tests/replica/two"
                        , parameters = [] : List Text
                        , testFile = "tests.json"
                        })
      with description = Some "Unknown commands are rejected, showing help"
  },
  { mapKey = "unknown_parameter"
  , mapValue =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                  , parameters = ["--oops"]
                                  , testFile = "tests.json"
                                  })
      with description = Some "If a parameter doesn't exist, display an error message and the help"
  },
  { mapKey = "opposite_include_exclude"
  , mapValue =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                  , parameters = ["--only one", "--exclude one"]
                                  , testFile = "tests.json"
                                  })
      with description = Some "If a test is both included and rejected, the command fails"
  },
  { mapKey = "opposite_include_exclude_tags"
  , mapValue =
      (Meta.replicaTest Meta.Run::{ directory = "tests/replica/two"
                                  , parameters = ["--tags shiny", "--exclude-tags shiny"]
                                  , testFile = "tests.json"
                                  })
      with description = Some "If a tag is both included and rejected, the command fails"
  }
  ]

in tests
