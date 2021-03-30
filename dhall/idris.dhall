let Replica = ./replica.dhall
let concatSep = https://prelude.dhall-lang.org/v20.1.0/Text/concatSep
let show = https://prelude.dhall-lang.org/v20.1.0/Text/show

let idris2_exe = (env:IDRIS2 as Text) ? "idris2"
-- if no env variable is present, we assume idris2 is in the path

let commonOptions = ["--no-color", "--console-width 0", "--no-banner"]

let Context : Type = < AutoPackage | Package : Text | File : Text | None >

let Input = < OneLiner : Text | MultiLine : Text >

let contextParam = \(c : Context) -> merge
  { AutoPackage = "--find-ipkg"
  , Package = \(str : Text) -> "--repl " ++ str
  , File = \(str : Text) -> str
  , None = ""
  } c

let Test : Type =
  { context : Context
  , input : Input
  , extraOptions : List Text
  }

let Minimal =
  { Type = Test
  , default =
    { extraOptions = [] : List Text
    }
  }

let buildCommand = \(t : Test) ->
  "${idris2_exe} ${concatSep " " (commonOptions # t.extraOptions)} ${contextParam t.context}"
  ++ merge  { OneLiner = \(str : Text) -> " --client ${show str}"
            , MultiLine = \(str : Text) -> ""} t.input

let clean = "rm -rf build"

let build : Test -> Replica.Test = \(t : Test) ->
  Replica.Minimal::{ command = buildCommand t
                   , input = merge { OneLiner = \(str : Text) -> None Text
                                   , MultiLine = \(str : Text) -> Some str} t.input
                   , afterTest = merge { AutoPackage = [clean]
                                       , Package = \(str : Text) -> [clean]
                                       , File = \(str : Text) -> [clean]
                                       , None = []: List Text
                                       } t.context
                   }

in {build, Test, Context, Input, Minimal}
