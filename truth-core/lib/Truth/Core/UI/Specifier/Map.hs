module Truth.Core.UI.Specifier.Map where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Specifier

data MapUISpec sel update where
    MkMapUISpec
        :: (forall w. CreateView selb updateB w -> CreateView sela updateA w)
        -> UISpec selb updateB
        -> MapUISpec sela updateA

instance Show (MapUISpec sel update) where
    show (MkMapUISpec _ spec) = "map " <> show spec

instance UIType MapUISpec where
    uiWitness = $(iowitness [t|MapUISpec|])

mapViewUISpec ::
       (forall w. CreateView selb updateB w -> CreateView sela updateA w) -> UISpec selb updateB -> UISpec sela updateA
mapViewUISpec mv spec = MkUISpec $ MkMapUISpec mv spec

shimViewUISpec :: CreateView sel update () -> UISpec sel update -> UISpec sel update
shimViewUISpec cvshim = mapViewUISpec $ \cvw -> cvshim >> cvw

mapUpdateUISpec ::
       forall sel updateA updateB. LifeCycleIO (EditLens updateA updateB) -> UISpec sel updateB -> UISpec sel updateA
mapUpdateUISpec getlens = mapViewUISpec $ cvMapEdit getlens

convertEditUISpec ::
       forall sel updateA updateB.
       ( IsEditUpdate updateA
       , IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => UISpec sel updateB
    -> UISpec sel updateA
convertEditUISpec = mapUpdateUISpec $ return convertEditLens

tupleEditUISpecs ::
       (TupleEditWitness FullEdit s, FiniteTupleSelector s)
    => (forall update. FullEdit (UpdateEdit update) => s update -> (UISpec sel update, t))
    -> [(UISpec sel (TupleUpdate s), t)]
tupleEditUISpecs getSpec =
    fmap
        (\(MkAnyW se) ->
             case tupleEditWitness @FullEdit se of
                 Dict ->
                     case getSpec se of
                         (spec, t) -> (mapUpdateUISpec (return $ tupleEditLens se) spec, t))
        tupleAllSelectors
