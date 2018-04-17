module Truth.Core.UI.Specifier.Lens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UILens edit where
    MkUILens :: forall edita editb. EditLens edita editb -> UISpec editb -> UILens edita

instance Show (UILens edit) where
    show (MkUILens _ uispec) = "lens " ++ show uispec

instance UIType UILens where
    uiWitness = $(iowitness [t|UILens|])

uiLens :: forall edita editb. EditLens edita editb -> UISpec editb -> UISpec edita
uiLens lens spec = MkUISpec $ MkUILens lens spec

uiConvert ::
       forall edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => UISpec editb
    -> UISpec edita
uiConvert = uiLens convertEditLens

mapUIWindow :: EditLens edita editb -> UIWindow editb -> UIWindow edita
mapUIWindow lens (MkUIWindow title content) = MkUIWindow (title . editLensFunction lens) (uiLens lens content)

ioMapAspect :: IO (EditLens edita editb) -> Aspect editb -> Aspect edita
ioMapAspect iolens aspect = do
    lens <- iolens
    mwin <- aspect
    return $ do
        win <- mwin
        return $ mapUIWindow lens win

mapAspect :: EditLens edita editb -> Aspect editb -> Aspect edita
mapAspect lens = ioMapAspect $ return lens

tupleEditUISpecs ::
       (TupleWitness FullEdit sel, FiniteTupleSelector sel)
    => (forall edit. FullEdit edit => sel edit -> (UISpec edit, t))
    -> [(UISpec (TupleEdit sel), t)]
tupleEditUISpecs getSpec =
    fmap
        (\(MkAnyWitness seledit) ->
             case tupleWitness @FullEdit seledit of
                 Dict ->
                     case getSpec seledit of
                         (spec, t) -> (MkUISpec $ MkUILens (tupleEditLens seledit) spec, t))
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

uiNothingValue :: Eq a => a -> UISpec (WholeEdit a) -> UISpec (WholeEdit (Maybe a))
uiNothingValue def = uiLens $ maybeNothingEditLens def
