module Replica.App.Info

import Control.ANSI
import Control.App
import Control.App.Console

import Data.List1
import Data.String

import Replica.App.FileSystem
import Replica.App.Log
import Replica.App.Replica
import Replica.Command.Info
import Replica.Core
import Replica.Option.Global
import Replica.Other.String

data InfoContext : Type where

displayTestName : Console e =>
  State GlobalConfig GlobalOption e =>
  String -> App e ()
displayTestName x = do
  ascii <- map ascii $ get GlobalConfig
  colour <- map colour $ get GlobalConfig
  let bullet = if ascii then "?" else "❓"
  let text = if colour then show (bolden "\{x}:") else "\{x}:"
  putStrLn "\{bullet} \{text}"

displayExpectation : FileSystem (FSError :: e) =>
  Has [ State InfoContext InfoAction
      , State GlobalConfig GlobalOption
      , State CurrentTest Test
      , Console] e => App e ()
displayExpectation = do
  t <- get CurrentTest
  f <- getExpectedFile
  handle (readFile f)
    (\o => do
      putStrLn $ withOffset 4 $ "Expected:"
      putStrLn !(expectation o))
    (\err : FSError => putStrLn "No expectation yet.")
  where
    expectation : String -> App e String
    expectation o = do
      c <- map colour $ get GlobalConfig
      let mod = if c then show . colored Blue else id
      pure $ unlines $ map (mod . withOffset 6) $ forget $ lines o

displayTests : FileSystem (FSError :: e) =>
  Has [ State InfoContext InfoAction
      , State GlobalConfig GlobalOption
      , State CurrentTest Test
      , Console] e =>
  App e ()
displayTests = do
  t <- get CurrentTest
  displayTestName t.name
  traverse_ (putStrLn . withOffset 4) t.description
  when (not $ null t.tags)
    $ putStrLn . withOffset 4 $ "Tags: \{show t.tags}"
  when (not $ null t.require)
    $ putStrLn . withOffset 4 $ "Require: \{show t.require}"
  putStrLn $ withOffset 4 "Command : \{show t.command}"
  when !(map showExpectation $ get InfoContext)
    displayExpectation
  putStrLn ""

export
infoReplica :
  FileSystem (FSError :: e) =>
  Has
    [ State InfoContext InfoAction
    , State GlobalConfig GlobalOption
    , Exception ReplicaError
    , Console
    ] e => App e ()
infoReplica = do
  repl <- getReplica InfoContext file
  debug $ show !(get GlobalConfig)
  putStrLn ""
  traverse_ (\t => new t displayTests) repl.tests
