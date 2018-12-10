module Test.Interactive
    ( getTestInteractive
    ) where

import Pinafore
import Pinafore.Test
import Shapes
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

testFile :: FilePath -> TestTree
testFile inpath = let
    rootpath = dropExtension inpath
    testname = takeBaseName rootpath
    refpath = rootpath <.> "ref"
    outpath = rootpath <.> "out"
    in goldenVsFile testname refpath outpath $
       withBinaryFile outpath WriteMode $ \outh ->
           withBinaryFile inpath ReadMode $ \inh -> do
               (pc, _) <- makeTestPinaforeContext
               pinaforeInteractHandles inh outh True pc
               hPutStrLn outh "<END>"

getTestInteractive :: IO TestTree
getTestInteractive = do
    inpaths <- findByExtension [".in"] $ "test" </> "interactive"
    return $ testGroup "interactive" $ fmap testFile inpaths
