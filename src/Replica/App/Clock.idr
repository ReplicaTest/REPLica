module Replica.App.Clock

import Control.App
import public System.Clock

%default total

public export
interface SystemClock e where
  threadClock : App e (Clock Thread)

export
PrimIO e => SystemClock e where
  threadClock = primIO $ clockTime Thread

export
showDuration : Clock Duration -> String
showDuration (MkClock seconds nanoseconds) = let
  ns = div nanoseconds 10000
  in "\{show seconds}.\{show ns}s"

export
durationOf : SystemClock e => App e a -> App e (Clock Duration, a)
durationOf x = do
  start <- threadClock
  res <- x
  end <- threadClock
  pure (timeDifference end start, res)
