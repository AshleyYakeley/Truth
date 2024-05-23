module Test.Library
    ( testLibrary
    ) where

import Pinafore.Libs
import Pinafore.Main
import Pinafore.Test.Internal
import Shapes
import Shapes.Test

testLibrary :: TestTree
testLibrary =
    testTree "library" $ let
        moExtraLibrary = extraLibrary
        moModuleDirs = []
        moduleNames :: [ModuleName]
        moduleNames = builtInModuleName : fmap lmName extraLibrary
        in let
               ?library = standardLibraryContext nullInvocationInfo MkModuleOptions {..}
               in for_ moduleNames $ \modname -> do
                      mmod <- fromInterpretResult $ runPinaforeScoped (show modname) $ lcLoadModule ?library modname
                      pmodule <- maybeToM (show modname <> ": not found") mmod
                      for_ (moduleScopeEntries pmodule) $ \(_, binfo) ->
                          case biValue binfo of
                              TypeBinding (MkSomeGroundType gt) -> let
                                  expected = show $ biOriginalName binfo
                                  satname = show gt
                                  names = filter ((/=) "_") $ words satname
                                  in case names of
                                         [found] -> assertEqual "type name" expected found
                                         _ -> fail $ "bad name: " <> satname
                              _ -> return ()
