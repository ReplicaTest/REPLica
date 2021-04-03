let Replica = ../dhall/replica.dhall
let Idris = ../dhall/idris.dhall

in [
  { mapKey = "nothingButALine"
  , mapValue = (Idris.oneLineTest { source = None Text
                                  , input = "Just \"Work\""
                                  , extraOptions = [] : List Text
                                  })
     with description = Some "Idris test without any library or loaded file"
     with tags = ["idris", "inline"]
  },
  { mapKey = "loadAFile"
  , mapValue = (Idris.oneLineTest { source = Some "Test.idr"
                                  , input = "aString"
                                  , extraOptions = [] : List Text
                                   })
     with workingDir = Some "tests/idris/file"
     with description = Some "Access a variable in a file"
     with tags = ["idris", "inline", "idris-file"]
  },
  { mapKey = "loadAPackage"
  , mapValue = (Idris.multiLineTest { context = Idris.Context.Package "test.ipkg"
                                    , input = ''
                                              :module Test
                                              aString
                                              :q
                                              ''
                                    , extraOptions = [] : List Text
                                    })
     with workingDir = Some "tests/idris/package"
     with description = Some "Load a package and access its content"
     with tags = ["idris", "inline", "idris-file"]
  }
  ]
