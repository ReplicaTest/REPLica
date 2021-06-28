module Replica.App.New

import Control.App
import Control.App.Console

import Data.String

import Language.JSON

import Replica.App.FileSystem
import Replica.App.Format
import Replica.App.Log
import Replica.App.Replica
import Replica.Command.New
import Replica.Other.Decorated

data NewContext : Type where

replicaURL : String
replicaURL = "https://raw.githubusercontent.com/ReplicaTest/REPLica/main/dhall/replica.dhall"

jsonTestSample : JSON
jsonTestSample = JObject
 [ ("command", JString "echo \"Hello, World!\"")
 , ("description", JString "This test is a placeholder, you can edit it.")
 , ("spaceSensitive", JBoolean False)
 , ("status", JBoolean True)
 , ("stdOut", JObject
     [ ("generated", JBoolean False)
     , ("consecutive", JArray $ JString <$> ["Hello", "World"])
     , ("end", JString "!")
     ])
  ]

dhallTestSample : String
dhallTestSample =
  #"""
   let hello = Replica.Success::
      { command = "echo \"Hello, World!\""
      , description = Some "This test is a placeholder, you can edit it."
      , spaceSensitive = False
      , stdOut = Replica.ComplexExpectation
          (Replica.EmptyExpectation::{consecutive = ["Hello", "World"], end = Some "!"})
      }
   """#

jsonContent : (withSample : Bool) -> JSON
jsonContent withSample =
  JObject $ if withSample then [("hello", jsonTestSample)] else []

dhallContent : (withSample : Bool) -> String
dhallContent withSample = unlines
  [ "let Replica = \{replicaURL}"
  , ""
  , if withSample
       then dhallTestSample
       else ""
  , ""
  , "let tests : Replica.Replica = \{sample}"
  , ""
  , "in tests"
  ]
  where
    sample : String
    sample = if withSample then "toMap { hello }" else "[] : Replica.Replica"

export
newReplica : FileSystem (FSError :: e) =>
  Has
    [ State NewContext NewCommand
    , Exception ReplicaError
    , Console
    ] e =>
  App e ()
newReplica = do
  ctx <- get NewContext
  let content = case ctx.format of
        JSON => show $ jsonContent ctx.includeSample
        Dhall => dhallContent ctx.includeSample
  catchNew (writeFile ctx.file content)
    (\err : FSError => throw (CantAccessTestFile ctx.file))
  putStrLn "Test file created (\{show ctx.format}): \{ctx.file}"
