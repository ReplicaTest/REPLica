module Replica.App.Info.Tag

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
import Replica.Command.Info.Tag
import Replica.Option.Filter
import Replica.Option.Global
import Replica.Other.Decorated
import Replica.Other.String

import Replica.App.Info.Types

displayTag :
  Has
    [ State GlobalConfig Global
    , Console
    ] e =>
  (Maybe String, List1 Test) -> App e ()
displayTag (name, tests) =
  putStrLn
    "\{!bold (maybe "- No tags" ("- " <+>) name)} (\{show $ length tests} tests)"

export
tagInfoReplica :
  FileSystem (FSError :: e) =>
  Has
    [ State TagInfoContext TagInfoCommand
    , State GlobalConfig Global
    , Exception ReplicaError
    , Console
    ] e => App e ()
tagInfoReplica = do
  ctx <- get TagInfoContext
  debug "Info: \{show ctx}"
  debug $ show !(get GlobalConfig)
  putStrLn ""
  tests <- fst <$> new ctx.filter defineActiveTests
  traverse_ displayTag $ byTag tests
