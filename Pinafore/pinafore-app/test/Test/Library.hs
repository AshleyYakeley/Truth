module Test.Library
    ( testLibrary
    )
where

import Pinafore.Main
import Pinafore.Test.Internal
import Shapes
import Shapes.Test

import Pinafore.Packages

keywords :: [Text]
keywords = fmap fst allKeywords

testLibrary :: TestTree
testLibrary =
    testTree "library" $ let
        moduleNames :: [ModuleName]
        moduleNames = fmap lmName $ packageLibraryModules appPackages
        in let
            ?library = createLibraryContext appPackages
            in for_ moduleNames $ \modname -> do
                mmod <-
                    fromInterpretResult
                        $ runPinaforeScoped (show modname)
                        $ runLoadModule (lcLoadModule ?library) modname
                pmodule <- maybeToM (show modname <> ": not found") mmod
                for_ (moduleScopeEntries pmodule) $ \(_, binfo) -> do
                    let oname = siOriginalName binfo
                    case siItem binfo of
                        TypeItem (MkSomeGroundType gt) -> let
                            expected = show oname
                            satname = show gt
                            names = filter ((/=) "_") $ words satname
                            in case names of
                                [found] -> assertEqual "type name" expected found
                                _ -> fail $ "bad name: " <> satname
                        _ -> return ()
                    if elem (showText $ fnName oname) keywords
                        then fail $ "keyword: " <> show modname <> ":" <> show oname
                        else return ()
