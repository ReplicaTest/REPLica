module Replica.App.Log

import Control.App
import Control.App.Console

import Replica.App.Replica
import Replica.Option.Global
import Replica.Other.Decorated

%default total

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
Console e => State GlobalConfig Global e => Log e where
  logWithLevel lvl content = do
    Just threshold <- map logLevel $ get GlobalConfig
      | Nothing => pure ()
    if lvl >= threshold
       then putStrLn content
       else pure ()
