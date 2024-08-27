module Pinafore.Context where

import Import
import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir

getPinaforeDir :: IO FilePath
getPinaforeDir = do
    mpdir <- lookupEnv "PINAFOREDIR"
    case mpdir of
        Just dir -> return dir
        Nothing -> getUserDataDir "pinafore"

setPinaforeDir :: Maybe FilePath -> IO ()
setPinaforeDir mdir = for_ mdir $ setEnv "PINAFOREDIR"

ensurePinaforeDir :: Maybe FilePath -> IO FilePath
ensurePinaforeDir mdir = do
    dir <-
        case mdir of
            Nothing -> getPinaforeDir
            Just dir -> return dir
    createDirectoryIfMissing True dir
    return dir
