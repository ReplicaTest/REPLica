let Replica = https://raw.githubusercontent.com/ReplicaTest/replica-dhall/main/package.dhall
let Test = Replica.Test
let Success = Replica.Status.Success

in { before = Test :: {beforeTest = ["oops"], command="true", workingDir = Some "getOut"}
   , later  = Test :: {command="true", workingDir = Some "getOut"}
   }
