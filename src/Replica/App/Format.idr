module Replica.App.Format

import public Control.ANSI
import Control.App

import Replica.Option.Global
import Replica.App.Log
import Replica.App.Replica
import Replica.Other.Decorated

export
ok : State GlobalConfig Global e => App e String
ok = do
  ascii <- map ascii $ get GlobalConfig
  pure $ if ascii then "OK " else "✅ "

export
ko : State GlobalConfig Global e => App e String
ko = do
  ascii <- map ascii $ get GlobalConfig
  pure $ if ascii then "KO " else "❌ "

export
err : State GlobalConfig Global e => App e String
err = do
  ascii <- map ascii $ get GlobalConfig
  pure $ if ascii then "ERR" else "⚠️ "

export
pending : State GlobalConfig Global e => App e String
pending = do
  ascii <- map ascii $ get GlobalConfig
  pure $ if ascii then "ZzZ" else "💤"

export
qmark : State GlobalConfig Global e => App e String
qmark = do
  ascii <- map ascii $ get GlobalConfig
  pure $ if ascii then "?" else "❓"

export
bold : State GlobalConfig Global e => App e (String -> String)
bold = do
  c <- map colour $ get GlobalConfig
  pure $ if c then (show . bolden) else id

export
yellow : State GlobalConfig Global e => App e (String -> String)
yellow = do
  c <- map colour $ get GlobalConfig
  pure $ if c then (show . colored Yellow) else id

export
green : State GlobalConfig Global e => App e (String -> String)
green = do
  c <- map colour $ get GlobalConfig
  pure $ if c then (show . colored Green) else id

export
red : State GlobalConfig Global e => App e (String -> String)
red = do
  c <- map colour $ get GlobalConfig
  pure $ if c then (show . colored Red) else id

export
blue : State GlobalConfig Global e => App e (String -> String)
blue = do
  c <- map colour $ get GlobalConfig
  pure $ if c then (show . colored BrightBlue) else id

