\(parameters : List Text) -> \(testFiles : List Text) ->
  ./toTest.dhall (./default.dhall // {command = "info", parameters, testFiles})
