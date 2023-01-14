module Replica.App.Info.Suite

import Control.App
import Control.App.Console

import Data.List
import Data.List1

import Replica.App.FileSystem
import Replica.App.Log
import Replica.App.Replica
import Replica.Core.Test
import Replica.Core.Types
import Replica.Command.Info.Suite
import Replica.Option.Global
import Replica.Other.Decorated

import Replica.App.Info.Types

export
suiteInfoReplica :
  FileSystem (FSError :: e) =>
  Has
    [ State SuiteInfoContext SuiteInfoCommand
    , State GlobalConfig Global
    , Exception ReplicaError
    , Console
    ] e => App e ()
suiteInfoReplica = do
  debug "Info: \{show !(get SuiteInfoContext)}"
  debug $ show !(get GlobalConfig)
  putStrLn ""
  tests <- ?defineActiveTests
  traverse_ ?displayTestBySuite $ bySuite tests
