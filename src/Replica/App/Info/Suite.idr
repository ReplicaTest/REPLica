module Replica.App.Info.Suite

import Control.App
import Control.App.Console

import Data.List
import Data.List1

import Replica.App.Display
import Replica.App.FileSystem
import Replica.App.Filter
import Replica.App.Format
import Replica.App.Log
import Replica.App.Replica
import Replica.Core.Test
import Replica.Core.Types
import Replica.Command.Info.Suite
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Other.Decorated
import Replica.Other.String

import Replica.Core.Types
import Replica.App.Info.Types

displaySuite :
  Has
    [ State GlobalConfig Global
    , Console
    ] e =>
  (Maybe String, List1 Test) -> App e ()
displaySuite (name, tests) =
  putStrLn
    "\{!bold (maybe "- No suite" ("- " <+>) name)} (\{show $ length tests} tests)"

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
  ctx <- get SuiteInfoContext
  debug "Info: \{show ctx}"
  debug $ show !(get GlobalConfig)
  putStrLn ""
  tests <- fst <$> new ctx.filter defineActiveTests
  traverse_ displaySuite $ bySuite tests
