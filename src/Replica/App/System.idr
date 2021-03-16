module Replica.App.System


import Control.App
import System

%default covering

public export
data SystemError = Err Int

public export
interface Has [Exception SystemError] e => SystemIO e where
  system : String -> App e ()

export
Has [PrimIO, Exception SystemError] e => SystemIO e where
  system exec = do
    0 <- primIO $ system exec
      | n => throw (Err n)
    pure ()
