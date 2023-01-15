module Replica.App.Run.Display

import Control.App
import Control.App.Console

import Data.List1
import Data.String

import Replica.App.Clock
import public Replica.App.Display
import Replica.App.Format
import Replica.App.Replica
import Replica.App.Run.Types
import Replica.App.System

import Replica.Core.Types
import Replica.Option.Global

import Replica.Command.Run
import Replica.Option.Global
import Replica.Other.Decorated
import Replica.Other.String

%default total

-- native way to display expectations
expectedVsGiven : State GlobalConfig Global e =>
  Nat -> String -> String -> App e (List String)
expectedVsGiven k expected given = pure $ map (withOffset k) $
  ( "Expected:" :: map !red (lines expected)) ++
  ( "Given:" :: map !green (lines given))

nativeShow : State GlobalConfig Global e =>
  Console e => Nat -> String -> String -> App e ()
nativeShow n expected given =
  traverse_ putStrLn !(expectedVsGiven n expected given)

-- Provide different ways to show the difference between expectations and givens
export
showDiff : SystemIO (SystemError :: e) =>
  State GlobalConfig Global e =>
  State CurrentTest Test e =>
  Console e => DiffCommand -> Nat -> String -> String -> App e ()
showDiff None n expected given = pure ()
showDiff Native n expected given = nativeShow n expected given
showDiff Diff n x y = catchNew
  (system $ "diff --minimal \{!getExpectedOutput} \{!getOutputFile}")
  (\err : SystemError => pure ())
showDiff GitDiff n x y = catchNew
  (system $ "git diff --minimal --word-diff=color --no-index -- \{!getExpectedOutput} \{!getOutputFile}")
  (\err : SystemError => pure ())
showDiff (Custom z) n x y = catchNew
  (system $ "\{z} \{!getExpectedOutput} \{!getOutputFile}")
  (\err : SystemError => pure ())

-- display the final report of a run
export
report : Console e => State GlobalConfig Global e => Stats -> App e ()
report x = do
  putStrLn $ separator 80
  putStrLn $ !bold "Summary:"
  let nb = countTests x
  if nb == 0
     then putStrLn $ withOffset 2 "No test"
     else putStrLn $ removeTrailingNL $ unlines $ catMaybes
    [ guard (x.successes > 0) $>
        withOffset 2 "\{!ok} (Success): \{show x.successes} / \{show nb}"
    , guard (x.failures > 0) $>
        withOffset 2 "\{!ko} (Failure): \{show x.failures} / \{show nb}"
    , guard (x.errors > 0) $>
        withOffset 2 "\{!err}  (Errors): \{show x.errors} / \{show nb}"
    , guard (x.skipped > 0) $>
        withOffset 2 "\{!pending}  (Pending): \{show x.skipped} / \{show nb}"
    ]

export
testOutput : SystemIO (SystemError :: e) =>
  Has [ State RunContext RunCommand
      , State GlobalConfig Global
      , State CurrentTest Test
      , Console
      ] e => Either TestError TestResult -> App e ()
testOutput (Left y) = do
  t <- get CurrentTest
  putStr (withOffset 2 $ (!yellow "\{!err} \{t.name}: "))
  putStrLn (displayTestError y)
testOutput (Right Skipped) = do
  t <- get CurrentTest
  putStrLn $ withOffset 2 "\{!pending} \{t.name}"
testOutput (Right (Success duration)) = do
  displayTime <- timing <$> get RunContext
  let time : String := if displayTime then " (\{showDuration duration})" else ""
  t <- get CurrentTest
  if !(hideSuccess <$> get RunContext)
     then pure ()
     else putStrLn $ withOffset 2 "\{!ok} \{t.name}\{time}"
testOutput (Right (Fail xs)) = do
  t <- get CurrentTest
  putStrLn $ withOffset 2 $ !red "\{!ko} \{t.name}:"
  traverse_ (putStrLn . withOffset 6 . !red) (xs >>= displayFailReason)
  traverse_ writeFailure xs
  where
    multilineDisplay : (offset : Nat) -> (content : String) -> App e ()
    multilineDisplay offset = traverse_ (putStrLn . withOffset offset) . lines

    displayError : (given : String)  -> (exp : (e: Expectation ** ExpectationError e)) -> App e ()
    displayError given (MkDPair (Exact x) snd) =
      let content = case !(diff <$> get GlobalConfig) of
            None => multilineDisplay 8 x
            Native => multilineDisplay 8 x
            d' => showDiff d' 8 given x
      in putStrLn (withOffset 6 "Exact expectation mismatch:") >> content
    displayError given (MkDPair (StartsWith x) snd) =
      putStrLn (withOffset 6 "Start mismatch:") >> multilineDisplay 4 x
    displayError given (MkDPair (EndsWith x) snd) =
      putStrLn (withOffset 6 "End mismatch:") >> multilineDisplay 4 x
    displayError given (MkDPair (Partial Ordered ys) snd) = do
      putStrLn (withOffset 6 "Consecutive expectations mismatch, first not found:")
      multilineDisplay 8 snd
    displayError given (MkDPair (Partial Whatever ys) snd) = do
      putStrLn (withOffset 6 "Contains expectations mismatch, not found:")
      traverse_ (multilineDisplay 8) snd
    displayError given (MkDPair Generated Nothing) = pure ()
    displayError given (MkDPair Generated (Just x)) =
      let content = case !(diff <$> get GlobalConfig) of
            None => multilineDisplay 8 x
            Native => multilineDisplay 8 x
            d' => showDiff d' 8 given x
      in putStrLn (withOffset 6 "Golden value expectation mismatch:") >> content

    writeFailure : FailReason -> App e ()
    writeFailure (WrongStatus _ expected) = pure ()
    writeFailure (ExpectedFileNotFound x) = pure ()
    writeFailure (WrongOutput x given ys) = do
      putStrLn $ withOffset 6 $ !bold "Error on \{displaySource x}:"
      traverse_ (displayError given) ys
      putStrLn $ withOffset 6 $ "Given:"
      traverse_ (putStrLn . withOffset 8) $ lines given

