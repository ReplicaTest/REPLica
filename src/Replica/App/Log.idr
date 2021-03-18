module Replica.App.Log

import Control.App
import Control.App.Console

import Replica.App.Replica
import Replica.Option.Global

%default total

export
data LogConfig : Type where

export
interface Log e where
  logWithLevel : (lvl : LogLevel) -> (content : String) -> App e ()

export
log : Log e => String -> App e ()
log = logWithLevel Info

export
debug : Log e => String -> App e ()
debug = logWithLevel Debug

export
warning : Log e => String -> App e ()
warning = logWithLevel Warning

export
critical : Log e => String -> App e ()
critical = logWithLevel Critical

export
Console e => State GlobalConfig GlobalOption e => Log e where
  logWithLevel lvl content = do
    Just threshold <- map logLevel $ get GlobalConfig
      | Nothing => pure ()
    if lvl <= threshold
       then putStrLn content
       else pure ()
