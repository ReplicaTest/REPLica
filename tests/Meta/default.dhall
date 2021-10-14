{ executable = "${env:PWD as Text}/build/exec/replica" ? "replica"
, command = "run -D"
, parameters = [] : List Text
, testFiles = [] : List Text
} : ./Type.dhall
