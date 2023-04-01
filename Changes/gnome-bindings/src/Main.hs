{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Data.GI.CodeGen.API
import Data.GI.CodeGen.CabalHooks
import Data.GI.CodeGen.Code
import Data.GI.CodeGen.CodeGen
import Data.GI.CodeGen.Config
import Data.GI.CodeGen.LibGIRepository
import Data.GI.CodeGen.ModulePath
import Data.GI.CodeGen.Overrides
import Data.GI.CodeGen.Util
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import qualified GI.GLib.Config as GLib
import Prelude

-- | Generate the code for the given module.
genModuleCode ::
       Text -- ^ name
    -> Text -- ^ version
    -> Text -- ^ pkgName
    -> Text -- ^ pkgVersion
    -> Bool -- ^ verbose
    -> [TaggedOverride] -- ^ Explicit overrides
    -> IO ModuleInfo
genModuleCode name version pkgName pkgVersion verbosity overrides = do
    setupTypelibSearchPath []
    parsed <-
        forM overrides $ \(TaggedOverride tag ovText) -> do
            parseOverrides ovText >>= \case
                Left err -> error $ "Error when parsing overrides file \"" <> T.unpack tag <> "\":" <> T.unpack err
                Right ovs -> return ovs
    let ovs = mconcat parsed
    (gir, girDeps) <- loadGIRInfo verbosity name (Just version) [] (girFixups ovs)
    let
        (apis, deps) = filterAPIsAndDeps ovs gir girDeps
        allAPIs = M.union apis deps
        cfg =
            Config
                { modName = name
                , modVersion = version
                , ghcPkgName = pkgName
                , ghcPkgVersion = pkgVersion
                , verbose = verbosity
                , overrides = ovs
                }
    return $ genCode cfg allAPIs (toModulePath name) (genModule apis)

main :: IO ()
main = do
    givenOvs <- traverse (\fname -> TaggedOverride (T.pack fname) <$> utf8ReadFile fname) overridesFile
    let ovs = maybe inheritedOverrides (: inheritedOverrides) givenOvs
    m <- genModuleCode name version pkgName pkgVersion verbose ovs
    _t <- writeModuleTree verbose outputDir m
    return ()
  where
    name = "GObject"
    version = "2.0"
    pkgName = "gi-gobject"
    pkgVersion = "2.0.28"
    overridesFile = Just "Changes/gnome-bindings/GObject.overrides"
    verbose = False
    outputDir = Just "Changes/gnome-bindings/result"
    inheritedOverrides = [TaggedOverride "inherited:GLib" GLib.overrides]
