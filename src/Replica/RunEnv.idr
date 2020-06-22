module Replica.RunEnv

%default total

public export
record RunEnv where
  constructor MkRunEnv
  interactive : Bool
  path : String
