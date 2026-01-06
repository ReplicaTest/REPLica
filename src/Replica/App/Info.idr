module Replica.App.Info

import Control.App
import Control.App.Console

import Data.List
import Data.String
import Data.String.Extra

import Language.JSON

import Replica.App.FileSystem
import Replica.App.Format
import Replica.App.Log
import Replica.App.Replica
import Replica.Command.Info
import Replica.Command.Info.Suite
import Replica.Command.Info.Tag
import Replica.Command.Info.Test
import Replica.Core
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Other.Decorated
import Replica.Other.String
import Replica.Other.Validation

import Replica.App.Info.Suite
import Replica.App.Info.Tag
import Replica.App.Info.Test
import public Replica.App.Info.Types

export
infoReplica :
  FileSystem (FSError :: e) =>
  Has
    [ State InfoContext InfoCommand
    , Exception ReplicaError
    , Console
    ] e => App e ()
infoReplica = do
  cmd <- get InfoContext
  case cmd of
    SuiteInfo x => new x.global $ new x $ suiteInfoReplica
    TagInfo x => new x.global $ new x $ tagInfoReplica
    TestInfo x => new x.global $ new x $ testInfoReplica
