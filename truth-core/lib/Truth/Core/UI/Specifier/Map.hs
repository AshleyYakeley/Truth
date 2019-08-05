module Truth.Core.UI.Specifier.Map where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Specifier

data MapUISpec sel edit where
    MkMapUISpec
        :: (forall w. CreateView selb editb w -> CreateView sela edita w) -> UISpec selb editb -> MapUISpec sela edita

instance Show (MapUISpec sel edit) where
    show (MkMapUISpec _ spec) = "map " <> show spec

instance UIType MapUISpec where
    uiWitness = $(iowitness [t|MapUISpec|])

mapViewUISpec ::
       (forall w. CreateView selb editb w -> CreateView sela edita w) -> UISpec selb editb -> UISpec sela edita
mapViewUISpec mv spec = MkUISpec $ MkMapUISpec mv spec

shimViewUISpec :: CreateView sel edit () -> UISpec sel edit -> UISpec sel edit
shimViewUISpec cvshim = mapViewUISpec $ \cvw -> cvshim >> cvw

mapEditUISpec :: forall sel edita editb. EditLens edita editb -> UISpec sel editb -> UISpec sel edita
mapEditUISpec lens = mapViewUISpec $ cvMapEdit lens

convertEditUISpec ::
       forall sel edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => UISpec sel editb
    -> UISpec sel edita
convertEditUISpec = mapEditUISpec convertEditLens

tupleEditUISpecs ::
       (TupleWitness FullEdit s, FiniteTupleSelector s)
    => (forall edit. FullEdit edit => s edit -> (UISpec sel edit, t))
    -> [(UISpec sel (TupleEdit s), t)]
tupleEditUISpecs getSpec =
    fmap
        (\(MkAnyW se) ->
             case tupleWitness @FullEdit se of
                 Dict ->
                     case getSpec se of
                         (spec, t) -> (mapEditUISpec (tupleEditLens se) spec, t))
        tupleAllSelectors

-- | not really a bijection
maybeNothingValueBijection :: Eq a => a -> Bijection (Maybe a) a
maybeNothingValueBijection def = let
    isoForwards (Just a) = a
    isoForwards Nothing = def
    isoBackwards a
        | a == def = Nothing
    isoBackwards a = Just a
    in MkIsomorphism {..}

maybeNothingEditLens :: Eq a => a -> EditLens (WholeEdit (Maybe a)) (WholeEdit a)
maybeNothingEditLens def = toEditLens $ maybeNothingValueBijection def

mapMaybeNothingUISpec :: Eq a => a -> UISpec sel (WholeEdit a) -> UISpec sel (WholeEdit (Maybe a))
mapMaybeNothingUISpec def = mapEditUISpec $ maybeNothingEditLens def
