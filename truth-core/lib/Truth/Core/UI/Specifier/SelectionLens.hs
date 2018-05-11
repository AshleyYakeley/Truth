module Truth.Core.UI.Specifier.SelectionLens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UISelectionLens seledit edit where
    MkUISelectionLens
        :: forall seledita seleditb edit.
           EditLens seledita seleditb
        -> UISpec seledita edit
        -> UISelectionLens seleditb edit

instance Show (UISelectionLens seledit edit) where
    show (MkUISelectionLens _ uispec) = "selection-lens " ++ show uispec

instance UIType UISelectionLens where
    uiWitness = $(iowitness [t|UISelectionLens|])

uiSelectionLens ::
       forall seledita seleditb edit. EditLens seledita seleditb -> UISpec seledita edit -> UISpec seleditb edit
uiSelectionLens lens spec = MkUISpec $ MkUISelectionLens lens spec

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
