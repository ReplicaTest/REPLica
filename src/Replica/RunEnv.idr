module Replica.RunEnv

%default total

public export
record RunEnv a where
  constructor MkRunEnv
  interactive : Bool
  value : a

public export
implementation Functor RunEnv where
  map func = record {value $= func}

export
setValue : a -> RunEnv b -> RunEnv a
setValue = map . const
