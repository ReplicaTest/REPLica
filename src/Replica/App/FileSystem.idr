module Replica.App.FileSystem


import Control.App
import System.Directory
import System.Path

%default covering

public export
data FSError
  = MissingFile String
  | CantAccess String
  | CantWriteFile String
  | CantReadFile String
  | CantCreate String
  | UnmanagedError String
  | FileExists String

export total
toFSError : FileError -> String -> FSError
toFSError (GenericFileError i) = UnmanagedError
toFSError FileReadError = CantReadFile
toFSError FileWriteError = CantWriteFile
toFSError FileNotFound = MissingFile
toFSError PermissionDenied = CantAccess
toFSError FileExists = FileExists

public export
interface Exception FSError e => FileSystem e where
  createDir : (dirname : String) -> App e ()
  getCurrentDir : App e String
  changeDir : (dirname : String) -> App e ()
  removeDir : (dirname : String) -> App e ()
  writeFile : (filename : String) -> (content : String) -> App e ()
  readFile : (filename : String) -> App e String

export
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
    case splitParent f of
      Just ("", filename) => pure ()
      Just (dir, filename) => buildDirectory dir
      Nothing => pure ()
    Right x <- primIO $ writeFile f content
      | Left err => throw (toFSError err f)
    pure x
    where
      buildDirectory : String -> App e ()
      buildDirectory dir = do
        Left _ <- primIO $ openDir dir
          | _ => pure ()
        let Just (parent, _) = splitParent dir
          | Nothing => throw (CantAccess dir)
        buildDirectory parent
        createDir dir
  readFile f = do
    Right x <- primIO $ readFile f
      | Left err => throw (toFSError err f)
    pure x
