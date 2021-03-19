let Replica = https://raw.githubusercontent.com/berewt/REPLica/main/dhall/replica.dhall

in { hello = Replica.Simple::{command = "echo \"Hello, world!\""}}
