module Replica.Types

import Control.App as App
import Control.App.Console as App

import Data.Maybe
import Data.List
import Data.List1
import Data.String
import Language.JSON

import System
import System.Directory as D

%default total

data Validation : (err, a : Type) -> Type where
  Valid : (x : a) -> Validation err a
  Error : (e : err) -> Validation err a

Functor (Validation err) where
  map f (Valid x) = Valid $ f x
  map f (Error x) = Error x

(Semigroup err) => Applicative (Validation err) where
  pure = Valid
  (Valid f)  <*> (Valid x)  = Valid $ f x
  (Valid _)  <*> (Error e)  = Error e
  (Error e)  <*> (Valid x)  = Error e
  (Error e1) <*> (Error e2) = Error $ e1 <+> e2

namespace Param

  record Param a where
    constructor MkParam
    name : String
    parser : String -> Maybe a

namespace Option

  prefixLongOption : String -> String
  prefixLongOption = ("--" <+>)

  prefixShortOption : Char -> String
  prefixShortOption x = pack ['-',x]

  record FlagOption a where
    constructor MkFlag
    changeLong : List1 String
    changeShort : List Char
    enforceLong : List String
    enforceShort : List Char
    description : String
    defaultValue : a
    invert : a -> a

  parseFlagOption : FlagOption a -> List String -> Maybe (a, List String)
  parseFlagOption o (x::xs) = let
    enforceParams = map prefixLongOption o.enforceLong
                  ++ map prefixShortOption o.enforceShort
    changeParams = map prefixLongOption (toList o.changeLong)
                 ++ map prefixShortOption o.changeShort
    in (guard (x `elem` enforceParams) $> (o.defaultValue, xs))
       <+> (guard (x `elem` changeParams) $> (o.invert o.defaultValue, xs))
  parseFlagOption _ _ = Nothing


  record ParamOption a where
    constructor MkOption
    longName : List1 String
    shortName : List Char
    description : String
    defaultValue : a
    param : (Param a)

  parseParamOption : ParamOption a -> List String -> Maybe (a, List String)
  parseParamOption o (x::y::xs) = let
    validOption = map prefixLongOption (toList o.longName)
                  ++ map prefixShortOption o.shortName
    in do
      guard (x `elem` validOption)
      map (flip MkPair xs) $ o.param.parser y
  parseParamOption _ _ = Nothing


namespace ReplicaContext

  testFile : Param String
  testFile = MkParam "filename" Just

  interactive : FlagOption Bool
  interactive = MkFlag
    ("interactive" ::: [])
    ['i']
    []
    []
    "(re)generate golden number if different/missing"
    False
    not

  workingDir : ParamOption String
  workingDir = MkOption
    ("working-dir" ::: ["wdir"])
    ['w']
    "set where the test are run"
    ".replica/test"
    (MkParam "dirName" Just)

  record RunAction' (f : Type -> Type) where
    constructor MkRunAction
    workingDir : f String
    interactive : f Bool
    file : f String

  RunAction : Type
  RunAction = RunAction' Prelude.id


  Semigroup (RunAction' List) where
    (<+>) (MkRunAction workingDirX interactiveX fileX)
          (MkRunAction workingDirY interactiveY fileY)
      = MkRunAction
          (workingDirX ++ workingDirY)
          (interactiveX ++ interactiveY)
          (fileX ++ fileY)

  Monoid (RunAction' List) where
    neutral = MkRunAction empty empty empty

  interactiveOption : Bool -> RunAction' List -> RunAction' List
  interactiveOption x = record {interactive $= (x::)}

  workingDirOption : String -> RunAction' List -> RunAction' List
  workingDirOption x = record {workingDir $= (x::)}

  fileOption : String -> RunAction' List -> RunAction' List
  fileOption x = record {file $= (x::)}

  parseRunOptions :
    List String -> RunAction' List ->
    Validation (List String) (RunAction' List)
  parseRunOptions [] a = ?validate a
  parseRunOptions xs@(x::tail) a = do
    let Just (f, xs')
        = map (mapFst interactiveOption) (parseFlagOption interactive xs)
          <|> map (mapFst workingDirOption) (parseParamOption workingDir xs)
          <|> (guard (tail == []) $> (fileOption x, tail))
      | Nothing => Error ["Unknnown option \{x}"]
    assert_total $ parseRunOptions xs' $ f a

  one : String -> List a -> Validation (List String) a
  one _ [z] = Valid z
  one x [] = Error ["\{x} is missing"]
  one x _ = Error ["\{x} is set several time"]

  oneWithDefault : String -> a -> List a -> Validation (List String) a
  oneWithDefault _ y [] = Valid y
  oneWithDefault _ _ [z] = Valid z
  oneWithDefault x y _ = Error ["\{x} is set several time"]

  validateRunAction :
    RunAction' List ->
    Validation (List String) RunAction
  validateRunAction (MkRunAction wd i f)
    = [|MkRunAction
      (oneWithDefault "workingDir" workingDir.defaultValue wd)
      (oneWithDefault "interactive" interactive.defaultValue i)
      (one "filename" f)
      |]

  parseAction : List String -> Maybe (Validation (List String) RunAction)
  parseAction ("run" :: xs)
    = Just $ case parseRunOptions xs neutral of
                  Valid x => validateRunAction x
                  Error e => Error e
  parseAction _ = Nothing

record Test where
  constructor MkTest
  name: String
  description: Maybe String
  -- workingDir : f $ Maybe String
  -- suite: f $ List String
  -- tags: f $ List String
  -- require : f $ List String
  beforeTest : List String
  afterTest : List String
  -- env: f $ Map String String
  command: String
  expectedStatus: Maybe Int

record Replica where
  constructor MkReplica
  tests: List Test

jsonToTest : String -> JSON -> Validation (List String) Test

jsonToTest str (JObject xs) =
  [| MkTest
  (pure str)
  (validateDesc $ lookup "description" xs)
  (validateBefore $ lookup "beforeTest" xs)
  (validateAfter $ lookup "afterTest" xs)
  (validateCommand $ lookup "command" xs)
  (validateStatus $ lookup "expectedStatus" xs)
  |]
  where

    validateDesc : Maybe JSON -> Validation (List String) (Maybe String)
    validateDesc Nothing = Valid empty
    validateDesc (Just JNull) = Valid empty
    validateDesc (Just (JString x)) = Valid $ Just $ x
    validateDesc (Just x)
      = Error ["Test description should be a string, found: \{show x}"]

    validateCommandItem : JSON -> Validation (List String) String
    validateCommandItem (JString x) = Valid x
    validateCommandItem x = Error ["An item must be a string, found \{show x}"]

    validateCommandList : (commandName : String) -> Maybe JSON -> Validation (List String) (List String)
    validateCommandList _ Nothing = Valid empty
    validateCommandList _ (Just JNull) = Valid empty
    validateCommandList _ (Just (JString x)) = Valid $ pure x
    validateCommandList _ (Just (JArray ys)) = traverse validateCommandItem ys
    validateCommandList cmd (Just x) = Error
      ["\{cmd} must contain a command (string) or an array of command, found \{show x}"]

    validateBefore : Maybe JSON -> Validation (List String) (List String)
    validateBefore = validateCommandList "beforeTest"

    validateAfter : Maybe JSON -> Validation (List String) (List String)
    validateAfter = validateCommandList "afterTest"

    validateCommand : Maybe JSON -> Validation (List String) String
    validateCommand k = ?h

    validateStatus : Maybe JSON -> Validation (List String) (Maybe Int)
    validateStatus Nothing = Valid empty
    validateStatus (Just JNull) = Valid empty
    validateStatus (Just (JNumber x)) = Valid $ Just $ cast x
    validateStatus (Just x) = Error ["Status should be an integer, found: \{show x}"]

jsonToTest str json = Error ["Expecting a JSON object for testa '\{str}' and got: \{show json}"]

jsonToReplica : JSON -> Validation (List String) Replica
jsonToReplica (JObject xs) = [| MkReplica $ traverse (uncurry jsonToTest) xs |]
jsonToReplica _ = Error ["Replica test file must be a JSON object"]

data FailReason : Type where
  WrongStatus : (expected, given : Int) -> FailReason
  WrongOutput : (expected, given : String) -> FailReason
  NoGolden : FailReason

data TestError
  = FileSystemError String
  | InitializationFailed String
  | WrapUpFailed String

data TestResult
  = Invalid String
  | Skipped
  | Success
  | Fail (List FailReason)

inDir : (dir : String) -> IO TestResult -> IO TestResult
inDir dir cmd = do
  Just cwd <- currentDir
    | Nothing => pure $ Invalid "Unable to get current directory"
  True <- changeDir dir
    | _ => pure $ Invalid "Unable to go to specified dir: \{dir}"
  res <- cmd
  True <- changeDir cwd
    | _ => pure $ Invalid "Unable to go back to current directory"
  pure res

namespace App

  defaultExpected : Test -> String
  defaultExpected t = "\{show t.name}.expected"

  defaultOutput : Test -> String
  defaultOutput t = "\{show t.name}.output"

  data FSError
    = MissingFile String
    | CantAccess String
    | CantWriteFile String
    | CantReadFile String
    | CantCreate String
    | UnmanagedError String
    | FileExists String

  toFSError : FileError -> String -> FSError
  toFSError (GenericFileError i) = UnmanagedError
  toFSError FileReadError = CantReadFile
  toFSError FileWriteError = CantWriteFile
  toFSError FileNotFound = MissingFile
  toFSError PermissionDenied = CantAccess
  toFSError FileExists = FileExists

  interface Exception FSError e => FileSystem e where
    covering
    createDir : (dirname : String) -> App e ()
    covering
    getCurrentDir : App e String
    covering
    changeDir : (dirname : String) -> App e ()
    covering
    removeDir : (dirname : String) -> App e ()
    covering
    writeFile : (filename : String) -> (content : String) -> App e ()
    covering
    readFile : (filename : String) -> App e String

  data SystemError = Err Int

  interface Has [Exception SystemError] e => SystemIO e where
    covering
    system : String -> App e ()

  Has [PrimIO, Exception SystemError] e => SystemIO e where
    system exec = do
      0 <- primIO $ system exec
        | n => throw (Err n)
      pure ()

  Has [PrimIO, Exception FSError] e => FileSystem e where
    createDir d = do
      Right x <- primIO $ createDir d
        | Left err => throw (toFSError err d)
      pure x
    getCurrentDir = do
      Just dir <- primIO currentDir
        | Nothing => throw (UnmanagedError "current dir")
      pure dir
    changeDir d = do
      res <- primIO $ changeDir d
      if res
         then pure ()
         else throw (CantAccess d)
    removeDir d = primIO $ removeDir d
    writeFile f content = do
      Right x <- primIO $ writeFile f content
        | Left err => throw (toFSError err f)
      pure x
    readFile f = do
      Right x <- primIO $ readFile f
        | Left err => throw (toFSError err f)
      pure x

  inDir : FileSystem (FSError :: es) =>
    String -> App es a -> App (FSError :: es) a
  inDir dir exec = do
    cwd <- getCurrentDir
    changeDir dir
    res <- lift exec
    changeDir cwd
    pure res

  runAll :
    SystemIO (SystemError :: e) =>
    Exception TestError e =>
    (String -> TestError) ->
    List String -> App e ()
  runAll  _ [] = pure ()
  runAll  liftError (x :: xs) =
    handle (system x)
      (const $ runAll liftError xs)
      (\err : SystemError => throw $ liftError x)

  data RunContext : Type where
  data CurrentTest : Type where

  expectedVsGiven : Console e => Maybe String -> String -> App e ()
  expectedVsGiven old given = do
    case old of
         Nothing => putStrLn "Expected: Nothing Found"
         Just str => do
           putStrLn "Expected:"
           putStrLn str
    putStrLn "Given:"
    putStrLn given


  askForNewGolden :
    FileSystem (FSError :: e) =>
    Has [ State CurrentTest Test
        , Exception TestError
        , Console
        ] e => Maybe String -> String -> App e TestResult
  askForNewGolden old given = do
    t <- get CurrentTest
    expectedVsGiven old given
    putStrLn $ "Do you want to " ++ maybe "set" (const "replace") old ++ " the golden value? [N/y]"
    if !readAnswer
       then do
         handle (writeFile (defaultExpected t) given)
           (const $ pure Success)
           (\err : FSError => throw $ FileSystemError "Cannot write golden value")
       else pure $ maybe (Fail [NoGolden])
                         (Fail . pure . flip WrongOutput given)
                         old
    where
      readAnswer : App e Bool
      readAnswer = do
        answer <- getLine
        pure $ toLower answer `elem` ["y", "yes"]


  data OutputError
    = ExpectedIsMissing
    | DifferentOutput String String

  checkOutput :
    FileSystem (FSError :: e) =>
    Has [ State CurrentTest Test
        , State RunContext RunAction
        , Exception TestError
        , Console ] e =>
    (expectedStatus : Maybe Int) -> (status : Int) ->
    (expectedOutput : Maybe String) -> (output : String) ->
    App e TestResult
  checkOutput expectedStatus status expectedOutput output
    = do
      ctx <- get RunContext
      maybe
        (pure $ maybe Success (Fail . pure) checkStatus)
        (\f => if ctx.interactive
                  then maybe (askForNewGolden expectedOutput output)
                             (\err => do
                                t <- askForNewGolden {e} expectedOutput output
                                pure $ case t of
                                           Fail xs => Fail (err :: xs)
                                           _ => Fail [err])
                             checkStatus
                  else pure . Fail $ maybe [f] (::[f]) checkStatus)
        checkExpectation
      where
        checkStatus : Maybe FailReason
        checkStatus = do
          s <- expectedStatus
          guard (s == status)
          pure $ WrongStatus s status
        checkExpectation : Maybe FailReason
        checkExpectation = do
          o <- expectedOutput
          guard (o == output)
          pure $ WrongOutput o output


  covering
  getExpected :
    FileSystem (FSError :: e) =>
    Has [ State CurrentTest Test
        , State RunContext RunAction
        , Exception TestError
        , Console
        ] e => String -> App e (Maybe String)
  getExpected given = do
    t <- get CurrentTest
    handle (readFile $ defaultExpected t)
      (pure . Just)
      (\err : FSError => case err of
          MissingFile _ => pure Nothing
          err => throw $ FileSystemError "Cannot read expectation")

  covering
  testCore :
    SystemIO (SystemError :: e) =>
    FileSystem (FSError :: e) =>
    Has [ State CurrentTest Test
        , State RunContext RunAction
        , Exception TestError
        , Console
        ] e => App e TestResult
  testCore = do
    t <- get CurrentTest
    exitStatus <- handle (system $ "\{t.command} >> \{defaultOutput t}")
      (const $ pure 0)
      (\(Err n) => pure n)
    output <- handle (readFile $ defaultOutput t) pure
      (\e : FSError => throw $
            FileSystemError "Can't read output file \{defaultOutput t}")
    expected <- getExpected output
    checkOutput t.expectedStatus exitStatus expected output

  covering
  performTest :
    SystemIO (SystemError :: e) =>
    FileSystem (FSError :: e) =>
    Has [ State CurrentTest Test
        , State RunContext RunAction
        , Exception TestError
        , Console
        ] e => App e TestResult
  performTest = do
    t <- get CurrentTest
    runAll InitializationFailed t.beforeTest
    res <- testCore
    runAll WrapUpFailed t.beforeTest
    pure res

  covering
  runTest :
    SystemIO (SystemError :: e) =>
    FileSystem (FSError :: e) =>
    Has [ State RunContext RunAction
        , State CurrentTest Test
        , Exception TestError
        , Console
        ] e => App e TestResult
  runTest = do
    ctx <- get RunContext
    handle (system "mkdir -p \{show ctx.workingDir}")
      pure
      (\err : SystemError => throw $ FileSystemError
        "Can't create working directory \{show ctx.workingDir}")
    handle (inDir ctx.workingDir performTest)
      pure
      (\err : FSError => throw $ FileSystemError
        "Error: cannot enter or exit test working directory \{show ctx.workingDir}")

  data ReplicaError
    = CanAccessTestFile String
    | InvalidJSON (List String)

  getReplica :
    FileSystem (FSError :: e) =>
    Has [ State RunContext RunAction
        , Exception ReplicaError ] e => App e Replica
  getReplica = do
    ctx <- get RunContext
    content <- handle (readFile ctx.file)
                      pure
                      (\err : FSError => throw $ CanAccessTestFile ctx.file)
    let Just json = parse content
          | Nothing => throw $ InvalidJSON []
    let Valid repl = jsonToReplica json
          | Error xs => throw $ InvalidJSON xs
    pure repl

  runReplica :
    SystemIO (SystemError :: TestError :: e) =>
    FileSystem (FSError :: TestError :: e) =>
    FileSystem (FSError :: e) =>
    Console (TestError :: e) =>
    Has [ State RunContext RunAction
        , Exception ReplicaError
        ] e => App e ()
  runReplica = do
    repl <- getReplica
    res <- runAllTests repl.tests
    traverse_ (either ?eh ?rh) (the (List $ Either TestError TestResult) res)
    where
      runAllTests : List Test -> App e (List (Either TestError TestResult))
      runAllTests [] = pure []
      runAllTests (x :: xs) = do
        r <- handle
               (new x runTest)
               (pure . Right)
               (\err : TestError => pure $ Left err)
        rs <- runAllTests xs
        pure $ r :: rs


  covering
  main : IO ()
  main = do
    (cmd::args) <- getArgs
      | _ => putStrLn "Error"
    let Just (Valid ctx) = parseAction args
      | Just (Error err) => do
          putStrLn $ unlines err
          exitWith {a = ()} $ ExitFailure 255
      | Nothing => case args of
                        [] => do
                           putStrLn "usage: replica run [options]"
                           exitWith $ ExitFailure 254
                        (x :: xs) => do
                           putStrLn "Unknown action \{show x}"
                           exitWith $ ExitFailure 254
    run $ new ctx $ handle runReplica pure (\err : ReplicaError => ?oek)
