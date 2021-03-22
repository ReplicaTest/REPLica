let Replica = ../../../dhall/replica.dhall

in { before = Replica.Minimal::{beforeTest = ["oops"], command="true", workingDir = Some "getOut"}
   , later  = Replica.Minimal::{command="true", workingDir = Some "getOut"}
   }
