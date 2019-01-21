module Truth.Core.UI.Specifier.Lens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UILens sel edit where
    MkUILens :: forall sel edita editb. EditLens edita editb -> UISpec sel editb -> UILens sel edita

instance Show (UILens sel edit) where
    show (MkUILens _ uispec) = "lens " ++ show uispec

instance UIType UILens where
    uiWitness = $(iowitness [t|UILens|])

uiLens :: forall sel edita editb. EditLens edita editb -> UISpec sel editb -> UISpec sel edita
uiLens lens spec = MkUISpec $ MkUILens lens spec

uiConvert ::
       forall sel edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => UISpec sel editb
    -> UISpec sel edita
uiConvert = uiLens convertEditLens

uiWindowMapEdit :: EditLens edita editb -> UIWindow editb -> UIWindow edita
uiWindowMapEdit lens (MkUIWindow title content action) =
    MkUIWindow (title . editLensFunction lens) (uiLens lens content) action

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
                         (spec, t) -> (MkUISpec $ MkUILens (tupleEditLens se) spec, t))
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

uiNothingValue :: Eq a => a -> UISpec sel (WholeEdit a) -> UISpec sel (WholeEdit (Maybe a))
uiNothingValue def = uiLens $ maybeNothingEditLens def
