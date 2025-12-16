let Meta = ./Meta/package.dhall

let Replica =
        env:REPLICA_DHALL
      ? https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
          sha256:83afdebd588461f9451e17d76551f27e7716774bd1ac610afe33e44b65228b28

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
          with suite = Some "ordering"
        , includeSuite =
            (Meta.Run ([ "-s A" ] : List Text) [ "crossSuiteDependency.json" ])
          with workingDir = Some "tests/replica/suite"
          with description = Some "can select only a given suite"
          with tags = [ "suite" ]
          with suite = Some "filter"
        , excludeSuite =
            (Meta.Run ([ "-S A" ] : List Text) [ "crossSuiteDependency.json" ])
          with workingDir = Some "tests/replica/suite"
          with description = Some "can exclude a give suite"
          with tags = [ "suite" ]
          with suite = Some "filter"
        }

in  tests
