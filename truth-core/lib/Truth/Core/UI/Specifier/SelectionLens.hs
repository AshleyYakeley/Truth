module Truth.Core.UI.Specifier.SelectionLens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UISetSelectionLens seledit edit where
    MkUISetSelectionLens
        :: forall seledita seleditb edit.
           EditLens seledita seleditb
        -> UISpec seledita edit
        -> UISetSelectionLens seleditb edit

instance Show (UISetSelectionLens seledit edit) where
    show (MkUISetSelectionLens _ uispec) = "selection-lens " ++ show uispec

instance UIType UISetSelectionLens where
    uiWitness = $(iowitness [t|UISetSelectionLens|])

uiSetSelectionLens ::
       forall seledita seleditb edit. EditLens seledita seleditb -> UISpec seledita edit -> UISpec seleditb edit
uiSetSelectionLens lens spec = MkUISpec $ MkUISetSelectionLens lens spec

uiAspectMapSelectionEdit :: EditLens seledita seleditb -> UIAspect seledita edit -> UIAspect seleditb edit
uiAspectMapSelectionEdit lens (MkUIAspect window asplens) = MkUIAspect window (lens . asplens)

aspectIOMapSelectionEdit :: IO (EditLens seledita seleditb) -> Aspect seledita edit -> Aspect seleditb edit
aspectIOMapSelectionEdit iolens aspect = do
    lens <- iolens
    mwin <- aspect
    return $ do
        win <- mwin
        return $ uiAspectMapSelectionEdit lens win

aspectMapSelectionEdit :: EditLens seledita seleditb -> Aspect seledita edit -> Aspect seleditb edit
aspectMapSelectionEdit lens = aspectIOMapSelectionEdit $ return lens

data UINoSelectionLens seledit edit where
    MkUINoSelectionLens :: UISpec seledita edit -> UINoSelectionLens seleditb edit

instance Show (UINoSelectionLens seledit edit) where
    show (MkUINoSelectionLens uispec) = "no-selection-lens " ++ show uispec

instance UIType UINoSelectionLens where
    uiWitness = $(iowitness [t|UINoSelectionLens|])

uiNoSelectionLens :: UISpec seledita edit -> UISpec seleditb edit
uiNoSelectionLens spec = MkUISpec $ MkUINoSelectionLens spec
