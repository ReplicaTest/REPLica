module Replica.Command.Version

import Data.List1

import Replica.Option.Types
import Replica.Version

public export
data Version = MkVersion String

export
parseVersion : List1 String -> ParseResult Version
parseVersion ("version" ::: xs) = Done $ MkVersion "replica version \{version}"
parseVersion xs = InvalidOption xs
