let Replica = ../dhall/replica.dhall
let Idris = ../dhall/idris.dhall

in [
  { mapKey = "nothingButALine"
  , mapValue = (Idris.build { context = Idris.Context.None
                            , input = Idris.Input.OneLiner "Just \"Work\""
                            , extraOptions = [] : List Text
                            })
     with description = Some "Idris test without any library or loaded file"
     with tags = ["idris", "inline"]
  },
  { mapKey = "loadAFile"
  , mapValue = (Idris.build { context = Idris.Context.File "Test.idr"
                            , input = Idris.Input.OneLiner "aString"
                            , extraOptions = [] : List Text
                            })
     with workingDir = Some "tests/idris/file"
     with description = Some "Access a variable in a file"
     with tags = ["idris", "inline", "idris-file"]
  },
  { mapKey = "loadAPackage"
  , mapValue = (Idris.build { context = Idris.Context.File "Test.idr"
                            , input = Idris.Input.OneLiner "aString"
                            , extraOptions = [] : List Text
                            })
     with workingDir = Some "tests/idris/file"
     with description = Some "Access a variable in a file"
     with tags = ["idris", "inline", "idris-file"]
  }
  ]
