module Test.Unifier
    ( testUnifier
    ) where

import Pinafore
import Pinafore.Language.API
import Shapes
import Test.RunScript

testLib :: LibraryModule
testLib = let
    specialA :: forall a. PinaforeAction ([a] -> [a])
    specialA = return id
    specialB :: forall a. a -> (a -> PinaforeAction ()) -> PinaforeAction ([a] -> [a]) -> PinaforeAction ()
    specialB v0 withVal mr = do
        liftIO $ hPutStrLn stderr "specialB ["
        r <- mr
        let
            l0 = [v0]
            l1 = r l0
            l2 = r l1
        for_ l2 withVal
        liftIO $ hPutStrLn stderr "specialB ]"
    specialMsgT :: Text -> PinaforeAction ()
    specialMsgT x = liftIO $ hPutStrLn stderr $ unpack x
    specialMsgI :: Integer -> PinaforeAction ()
    specialMsgI x = liftIO $ hPutStrLn stderr $ show x
    in MkDocTree "TEST" "" $
       [ mkValEntry "specialA" "TEST" $ specialA @X
       , mkValEntry "specialB" "TEST" $ specialB @Y
       , mkValEntry "specialMsgT" "TEST" $ specialMsgT
       , mkValEntry "specialMsgI" "TEST" $ specialMsgI
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
              [ tModify (failTestBecause "ISSUE 108") $ testExpectSuccess "specialB \"PQPQPQ\" specialMsgT specialA"
              , tModify (failTestBecause "ISSUE 108") $ testExpectSuccess "specialB 10 specialMsgI specialA"
              ]
        ]
