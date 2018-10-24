module Truth.Core.UI.Specifier.Lens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UILens seledit edit where
    MkUILens :: forall seledit edita editb. EditLens edita editb -> UISpec seledit editb -> UILens seledit edita

instance Show (UILens seledit edit) where
    show (MkUILens _ uispec) = "lens " ++ show uispec

instance UIType UILens where
    uiWitness = $(iowitness [t|UILens|])

uiLens :: forall seledit edita editb. EditLens edita editb -> UISpec seledit editb -> UISpec seledit edita
uiLens lens spec = MkUISpec $ MkUILens lens spec

uiConvert ::
       forall seledit edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => UISpec seledit editb
    -> UISpec seledit edita
uiConvert = uiLens convertEditLens

uiWindowMapEdit :: EditLens edita editb -> UIWindow editb -> UIWindow edita
uiWindowMapEdit lens (MkUIWindow title content) = MkUIWindow (title . editLensFunction lens) (uiLens lens content)

uiAspectMapEdit :: EditLens edita editb -> UIAspect seledit editb -> UIAspect seledit edita
uiAspectMapEdit lens (MkUIAspect window asplens) = MkUIAspect (uiWindowMapEdit lens window) (asplens . lens)

aspectIOMapEdit :: IO (EditLens edita editb) -> Aspect seledit editb -> Aspect seledit edita
aspectIOMapEdit iolens aspect = do
    lens <- iolens
    mwin <- aspect
    return $ do
        win <- mwin
        return $ uiAspectMapEdit lens win

aspectMapEdit :: EditLens edita editb -> Aspect seledit editb -> Aspect seledit edita
aspectMapEdit lens = aspectIOMapEdit $ return lens

tupleEditUISpecs ::
       (TupleWitness FullEdit sel, FiniteTupleSelector sel)
    => (forall edit. FullEdit edit => sel edit -> (UISpec seledit edit, t))
    -> [(UISpec seledit (TupleEdit sel), t)]
tupleEditUISpecs getSpec =
    fmap
        (\(MkAnyW seledit) ->
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

uiNothingValue :: Eq a => a -> UISpec seledit (WholeEdit a) -> UISpec seledit (WholeEdit (Maybe a))
uiNothingValue def = uiLens $ maybeNothingEditLens def
