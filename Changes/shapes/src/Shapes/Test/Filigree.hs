module Shapes.Test.Filigree (filigreeVsFile) where

import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced

import Shapes

section :: [String] -> Result String ([String], [String])
section [] = throwExc "unterminated section in reference"
section ("{{{/}}}" : pp) = return ([], pp)
section (p : pp) = do
    (s, pp') <- section pp
    return (p : s, pp')

takeLine :: String -> [String] -> Result String [String]
takeLine s [] = throwExc $ "missing line " <> s
takeLine s (f : ff) | s == f = return ff
takeLine s (f : ff) = do
    ff' <- takeLine s ff
    return $ f : ff'

filigreeSection :: [String] -> [String] -> [String] -> Result String ()
filigreeSection [] pp ff = filigree pp ff
filigreeSection (s : ss) pp ff = do
    ff' <- takeLine s ff
    filigreeSection ss pp ff'

filigree :: [String] -> [String] -> Result String ()
filigree [] [] = return ()
filigree [] (_ : _) = throwExc "excess lines"
filigree ("{{{unordered}}}" : pp) ff = do
    (sec, pp') <- section pp
    filigreeSection sec pp' ff
filigree (_ : _) [] = throwExc "missing lines"
filigree (p : pp) (f : ff) | p == f = filigree pp ff
filigree (p : _) (f : _) = throwExc $ "expected " <> show p <> ", found " <> show f

filigreeVsFile :: TestName -> FilePath -> FilePath -> IO () -> TestTree
filigreeVsFile testName refPath outPath action =
    let
        getRefPattern :: IO [String]
        getRefPattern = do
            bs <- readFile refPath
            return $ lines $ unpack $ decodeUtf8 bs

        getOutput :: IO [String]
        getOutput = do
            action
            bs <- readFile outPath
            return $ lines $ unpack $ decodeUtf8 bs

        toIOMS :: Result String () -> IO (Maybe String)
        toIOMS rs = return $ case rs of
            FailureResult s -> Just s
            SuccessResult () -> Nothing

        match :: [String] -> [String] -> IO (Maybe String)
        match pp ff = toIOMS $ filigree pp ff

        updateRefFile :: [String] -> IO ()
        updateRefFile newOutputLines =
            createDirectoriesAndWriteFile refPath $ encodeUtf8 $ pack $ unlines newOutputLines

        deleteOutFile :: IO ()
        deleteOutFile = removeFile outPath
        in goldenTest2 testName getRefPattern getOutput match updateRefFile deleteOutFile
