let Meta = ./Meta/package.dhall

let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:b11ac5d5195183145bbff03ba7b99e98b4e1bce32c725af5bedf01b4b328a741

let tests
    : Replica.Type
    = toMap
        { simpleDisplay =
            (Meta.Run ([] : List Text) [ "simpleDisplay.json" ])
          with workingDir = Some "tests/replica/suite"
          with description = Some "tests are ran by suite"
          with tags = [ "suite" ]
        , crossSuiteDependency =
            (Meta.Run ([] : List Text) [ "crossSuiteDependency.json" ])
          with workingDir = Some "tests/replica/suite"
          with description = Some "require can work on different suites"
          with tags = [ "suite" ]
        , includeSuite =
            (Meta.Run ([ "-s A" ] : List Text) [ "crossSuiteDependency.json" ])
          with workingDir = Some "tests/replica/suite"
          with description = Some "require can work on different suites"
          with tags = [ "suite" ]
        , excludeSuite =
            (Meta.Run ([ "-S A" ] : List Text) [ "crossSuiteDependency.json" ])
          with workingDir = Some "tests/replica/suite"
          with description = Some "require can work on different suites"
          with tags = [ "suite" ]
        }

in  tests
