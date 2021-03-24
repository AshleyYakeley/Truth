module Test.Unifier
    ( testUnifier
    ) where

import Pinafore
import Pinafore.Language.API
import Shapes
import Test.RunScript

testLib :: LibraryModule
testLib = let
    f :: forall a. a -> (a -> PinaforeAction ()) -> (a -> a) -> PinaforeAction ()
    f v withVal r = do
        liftIO $ hPutStrLn stderr "f ["
        withVal $ r $ r v
        liftIO $ hPutStrLn stderr "f ]"
    msgT :: Text -> PinaforeAction ()
    msgT x = liftIO $ hPutStrLn stderr $ unpack x
    msgI :: Integer -> PinaforeAction ()
    msgI x = liftIO $ hPutStrLn stderr $ show x
    in MkDocTree "TEST" "" $
       [ mkValEntry "i" "TEST" $ (id :: Text -> Text)
       , mkValEntry "f" "TEST" $ f @X
       , mkValEntry "msgT" "TEST" $ msgT
       , mkValEntry "msgI" "TEST" $ msgI
       ]

testUnifier :: TestTree
testUnifier =
    runScriptTestTree $
    tGroup
        "unifier"
        [ tLibrary testLib $
          tDecls ["import TEST"] $
          tGroup
              "ISSUE 108"
              [ tModify (failTestBecause "ISSUE 108") $ testExpectSuccess "f \"PQPQPQ\" msgT i"
              , tModify (failTestBecause "ISSUE 108") $ testExpectSuccess "f \"PQPQPQ\" msgT id"
              , tModify (failTestBecause "ISSUE 108") $ testExpectSuccess "f 10 msgI id"
              ]
        ]
