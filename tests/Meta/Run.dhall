\(parameters : List Text) -> \(testFiles : List Text) ->
  ./toTest.dhall (./default.dhall // {parameters, testFiles})
