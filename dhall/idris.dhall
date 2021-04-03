let Replica = ./replica.dhall

let fold =
      https://prelude.dhall-lang.org/v20.1.0/Optional/fold.dhall

let concatSep =
      https://prelude.dhall-lang.org/v20.1.0/Text/concatSep.dhall sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let show =
      https://prelude.dhall-lang.org/v20.1.0/Text/show.dhall sha256:c9dc5de3e5f32872dbda57166804865e5e80785abe358ff61f1d8ac45f1f4784

let idris2_exe = env:IDRIS2 as Text ? "idris2"

let Context
    : Type
    = < Package : Text | File : Text | None >

let contextParam =
      \(c : Context) ->
        merge
          { Package = \(str : Text) -> ["--repl ", str]
          , File = \(str : Text) -> [str]
          , None = [] : List Text
          }
          c

let OneLineContent =
      { source : Optional Text, input : Text, extraOptions : List Text }

let MultiLineContent =
      { context : Context, input : Text, extraOptions : List Text }

let Test
    : Type
    = < OneLine : OneLineContent | MultiLine : MultiLineContent >

let getContext
    : Test -> Context
    = \(t : Test) ->
    merge
      { OneLine = \(x : OneLineContent) ->
          (fold Text x.source Context (\(f : Text) -> Context.File f) Context.None)
      , MultiLine = \(x : MultiLineContent) -> x.context
      }
      t

let getInput
    : Test -> Optional Text
    = \(t : Test) ->
    merge
      { OneLine   = \(x : OneLineContent) -> None Text
      , MultiLine = \(x : MultiLineContent) -> Some x.input
      }
      t

let buildOneLineCommand
    : OneLineContent -> List Text
    = \(t : OneLineContent) ->
        [idris2_exe] # t.extraOptions #
        (fold Text t.source (List Text) (\(file : Text) -> [file]) ([] : List Text)) #
        ["--client ${show t.input}"]

let buildMultiLineCommand
    : MultiLineContent -> List Text
    = \(t : MultiLineContent) ->
        [idris2_exe] # t.extraOptions #
        (contextParam t.context)


let buildCommand =
      \(t : Test) ->
            concatSep " " (merge
              { OneLine = buildOneLineCommand
              , MultiLine = buildMultiLineCommand
              }
              t)

let clean = "rm -rf build"

let build
    : Test -> Replica.Test
    = \(t : Test) ->
        Replica.Minimal::{
        , command = buildCommand t
        , input = getInput t
        , afterTest =
            merge
              { Package = \(str : Text) -> [ clean ]
              , File = \(str : Text) -> [ clean ]
              , None = [] : List Text
              }
              (getContext t)
        }

let oneLineTest
    : OneLineContent -> Replica.Test
    = \(content : OneLineContent) -> build (Test.OneLine content)

let multiLineTest
    : MultiLineContent -> Replica.Test
    = \(content : MultiLineContent) -> build (Test.MultiLine content)

in  { build, Test, Context, OneLineContent, MultiLineContent, oneLineTest, multiLineTest }
