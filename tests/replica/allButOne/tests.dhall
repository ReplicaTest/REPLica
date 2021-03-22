let Replica = ../../../dhall/replica.dhall

in {
   , unfortunate1 = Replica.Minimal::{command = "false"}
       with succeed = Some True
   , unfortunate2 = Replica.Minimal::{command = "false"}
       with succeed = Some True
   , unfortunate3 = Replica.Minimal::{command = "false"}
       with succeed = Some True
   , theChosen = Replica.Minimal::{command = "true"}
       with succeed = Some True
   }
