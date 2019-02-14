module Truth.Core.UI.Specifier.Map where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data MapUISpec sel edit where
    MkMapUISpec :: forall sel edita editb. EditLens edita editb -> UISpec sel editb -> MapUISpec sel edita

instance Show (MapUISpec sel edit) where
    show (MkMapUISpec _ uispec) = "lens " ++ show uispec

instance UIType MapUISpec where
    uiWitness = $(iowitness [t|MapUISpec|])

mapUISpec :: forall sel edita editb. EditLens edita editb -> UISpec sel editb -> UISpec sel edita
mapUISpec lens spec = MkUISpec $ MkMapUISpec lens spec

convertUISpec ::
       forall sel edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => UISpec sel editb
    -> UISpec sel edita
convertUISpec = mapUISpec convertEditLens

mapWindowSpec :: EditLens edita editb -> WindowSpec editb -> WindowSpec edita
mapWindowSpec lens (MkWindowSpec title content) = MkWindowSpec (title . editLensFunction lens) (mapUISpec lens content)

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
                         (spec, t) -> (MkUISpec $ MkMapUISpec (tupleEditLens se) spec, t))
        tupleAllSelectors

-- | not really a bijection
maybeNothingValueBijection :: Eq a => a -> Bijection (Maybe a) a
maybeNothingValueBijection def = let
    biForwards (Just a) = a
    biForwards Nothing = def
    biBackwards a
        | a == def = Nothing
    biBackwards a = Just a
    in MkBijection {..}

maybeNothingEditLens :: Eq a => a -> EditLens (WholeEdit (Maybe a)) (WholeEdit a)
maybeNothingEditLens def = toEditLens $ maybeNothingValueBijection def

mapMaybeNothingUISpec :: Eq a => a -> UISpec sel (WholeEdit a) -> UISpec sel (WholeEdit (Maybe a))
mapMaybeNothingUISpec def = mapUISpec $ maybeNothingEditLens def
