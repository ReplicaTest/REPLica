let Replica = ../../../dhall/replica.dhall

let tests =
  { root_failed = Replica.Minimal::{command = "false"}
      with succeed = Some True
  , depends_failed = Replica.Minimal::{command = "true"}
      with require = ["root_failed"]
      with succeed = Some True
  }

in tests
