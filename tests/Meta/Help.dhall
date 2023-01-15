\(parameters : List Text) ->
  ./toTest.dhall
    (     ./default.dhall
      //  { command = "help", parameters, testFiles = [] : List Text }
    )
