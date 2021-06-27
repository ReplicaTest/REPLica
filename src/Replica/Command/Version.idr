module Replica.Command.Version

import Data.List1

import Replica.Help
import Replica.Option.Types
import Replica.Version

public export
data Version = MkVersion String

export
parseVersion : List1 String -> ParseResult Version
parseVersion ("version" ::: xs) = Done $ MkVersion "replica version \{version}"
parseVersion xs = InvalidOption xs

export
helpVersion : Help
helpVersion = MkHelp
  "version" (Just "replica version")
  "Show replica version"
  []
  Nothing
