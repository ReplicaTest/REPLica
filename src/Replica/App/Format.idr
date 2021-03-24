module Replica.App.Format

import public Control.ANSI
import Control.App

import Replica.Option.Global
import Replica.App.Log
import Replica.App.Replica

export
ok : State GlobalConfig GlobalOption e => App e String
ok = do
  ascii <- map ascii $ get GlobalConfig
  pure $ if ascii then "OK " else "✅ "

export
ko : State GlobalConfig GlobalOption e => App e String
ko = do
  ascii <- map ascii $ get GlobalConfig
  pure $ if ascii then "KO " else "❌ "

export
err : State GlobalConfig GlobalOption e => App e String
err = do
  ascii <- map ascii $ get GlobalConfig
  pure $ if ascii then "ERR" else "⚠️ "

export
qmark : State GlobalConfig GlobalOption e => App e String
qmark = do
  ascii <- map ascii $ get GlobalConfig
  pure $ if ascii then "?" else "❓"

export
bold : State GlobalConfig GlobalOption e => App e (String -> String)
bold = do
  c <- map colour $ get GlobalConfig
  pure $ if c then (show . bolden) else id

export
yellow : State GlobalConfig GlobalOption e => App e (String -> String)
yellow = do
  c <- map colour $ get GlobalConfig
  pure $ if c then (show . colored Yellow) else id

export
green : State GlobalConfig GlobalOption e => App e (String -> String)
green = do
  c <- map colour $ get GlobalConfig
  pure $ if c then (show . colored Green) else id

export
red : State GlobalConfig GlobalOption e => App e (String -> String)
red = do
  c <- map colour $ get GlobalConfig
  pure $ if c then (show . colored Red) else id

export
blue : State GlobalConfig GlobalOption e => App e (String -> String)
blue = do
  c <- map colour $ get GlobalConfig
  pure $ if c then (show . colored BrightBlue) else id

