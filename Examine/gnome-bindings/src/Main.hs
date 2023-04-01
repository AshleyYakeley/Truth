{-# OPTIONS -fno-warn-orphans #-}
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
import qualified GI.GLib.Config as GLib
import Shapes

instance Show TaggedOverride where
    show tov = unpack (overrideTag tov) <> ": " <> unpack (overrideText tov)

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
                Left err -> error $ "Error when parsing overrides file \"" <> unpack tag <> "\":" <> unpack err
                Right ovs -> return ovs
    let
        ovs = mconcat parsed
        cfg :: Config
        cfg =
            Config
                { modName = name
                , modVersion = version
                , ghcPkgName = pkgName
                , ghcPkgVersion = pkgVersion
                , verbose = verbosity
                , overrides = ovs
                }
    (gir, girDeps) <- loadGIRInfo verbosity name (Just version) ["Examine/gnome-bindings/new"] (girFixups ovs)
    let
        (apis, deps) = filterAPIsAndDeps ovs gir girDeps
        allAPIs :: Map Name API
        allAPIs = union apis deps
        cg :: CodeGen e ()
        cg = genModule apis
        mi :: ModuleInfo
        mi = genCode cfg allAPIs (toModulePath name) cg
    for_ (mapToList allAPIs) $ \case
        (n, APIFunction f) ->
            case fnSymbol f of
                "g_signal_add_emission_hook" -> putStrLn $ show (n, f)
                _ -> return ()
        _ -> return ()
    return mi

main :: IO ()
main = do
    givenOvs <- traverse (\fname -> TaggedOverride (pack fname) <$> utf8ReadFile fname) overridesFile
    let ovs = maybe inheritedOverrides (: inheritedOverrides) givenOvs
    m <- genModuleCode name version pkgName pkgVersion verbose ovs
    _t <- writeModuleTree verbose outputDir m
    return ()
  where
    name = "GObject"
    version = "2.0"
    pkgName = "gi-gobject"
    pkgVersion = "2.0.28"
    overridesFile = Just "Examine/gnome-bindings/GObject.overrides"
    verbose = True
    outputDir = Just "Examine/gnome-bindings/result"
    inheritedOverrides = [TaggedOverride "inherited:GLib" GLib.overrides]
