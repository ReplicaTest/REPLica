module Replica.Other.String

import Data.List
import Data.String

%default total

export
separator : Nat -> String
separator = pack . flip replicate '-'

export
withOffset : Nat -> String -> String
withOffset k = (++) (pack $ replicate k ' ')
