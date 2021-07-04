{ executable = "${env:PWD as Text}/build/exec/replica" ? "replica"
, command = "run"
, parameters = [] : List Text
, testFiles = [] : List Text
} : ./Type.dhall
