module Truth.Core.UI.Specifier.Map where

import Truth.Core.Import
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Specifier

data MapUISpec sel where
    MkMapUISpec :: (forall w. CreateView selb w -> CreateView sela w) -> UISpec selb -> MapUISpec sela

instance Show (MapUISpec sel) where
    show (MkMapUISpec _ spec) = "map " <> show spec

instance UIType MapUISpec where
    uiWitness = $(iowitness [t|MapUISpec|])

mapViewUISpec :: (forall w. CreateView selb w -> CreateView sela w) -> UISpec selb -> UISpec sela
mapViewUISpec mv spec = MkUISpec $ MkMapUISpec mv spec

shimViewUISpec :: CreateView sel () -> UISpec sel -> UISpec sel
shimViewUISpec cvshim = mapViewUISpec $ \cvw -> cvshim >> cvw
{-
tupleEditUISpecs ::
       (TupleEditWitness FullEdit s, FiniteTupleSelector s)
    => (forall . FullEdit (UpdateEdit ) => s  -> (UISpec sel , t))
    -> [(UISpec sel (TupleUpdate s), t)]
tupleEditUISpecs getSpec =
    fmap
        (\(MkAnyW se) ->
             case tupleEditWitness @FullEdit se of
                 Dict ->
                     case getSpec se of
                         (spec, t) -> (mapUpdateUISpec (return $ tupleEditLens se) spec, t))
        tupleAllSelectors
-}
