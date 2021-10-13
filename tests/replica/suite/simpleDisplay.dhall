let Replica = https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall

let Test = Replica.Test
let Status = Replica.Status
let Expectation = Replica.Expectation

let tests
    : Replica.Type
    = toMap
    { oneA = Test.Success ::
      { command = "true"
      , suite = Some "A"
      , stdOut = Expectation.Skipped
      }
    , oneB = Test.Success ::
      { command = "true"
      , suite = Some "B"
      , stdOut = Expectation.Skipped
      }
    , secondA = Test.Success ::
      { command = "true"
      , suite = Some "A"
      , stdOut = Expectation.Skipped
      }
    }

in tests
