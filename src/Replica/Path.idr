module Replica.Path

import public Data.List

%default total

public export
Path : Type
Path = DPair (List String) NonEmpty

