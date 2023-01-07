||| Some utils for string manipulation
module Replica.Other.String

import Data.List
import Data.String
import Data.String.Extra

%default total

export
separator : Nat -> String
separator = pack . flip replicate '-'

export
withOffset : Nat -> String -> String
withOffset k = (++) (pack $ replicate k ' ')

export
removeTrailingNL : String -> String
removeTrailingNL str = if "\n" `isSuffixOf` str then dropLast 1 str else str
