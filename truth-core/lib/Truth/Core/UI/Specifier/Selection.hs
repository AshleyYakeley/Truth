module Truth.Core.UI.Specifier.Selection where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data MapSelectionUISpec sel edit where
    MkMapSelectionUISpec :: forall sela selb edit. (sela -> selb) -> UISpec sela edit -> MapSelectionUISpec selb edit

instance Show (MapSelectionUISpec sel edit) where
    show (MkMapSelectionUISpec _ uispec) = "selection-lens " ++ show uispec

instance UIType MapSelectionUISpec where
    uiWitness = $(iowitness [t|MapSelectionUISpec|])

mapSelectionUISpec :: forall sela selb edit. (sela -> selb) -> UISpec sela edit -> UISpec selb edit
mapSelectionUISpec f spec = MkUISpec $ MkMapSelectionUISpec f spec

ioMapSelectionAspect :: IO (sela -> selb) -> Aspect sela -> Aspect selb
ioMapSelectionAspect iof aspect = do
    f <- iof
    msel <- aspect
    return $ do
        sel <- msel
        return $ f sel

mapSelectionAspect :: (sela -> selb) -> Aspect sela -> Aspect selb
mapSelectionAspect f = ioMapSelectionAspect $ return f

data NoSelectionUISpec sel edit where
    MkNoSelectionUISpec :: UISpec sela edit -> NoSelectionUISpec selb edit

instance Show (NoSelectionUISpec sel edit) where
    show (MkNoSelectionUISpec uispec) = "no-selection-lens " ++ show uispec

instance UIType NoSelectionUISpec where
    uiWitness = $(iowitness [t|NoSelectionUISpec|])

noSelectionUISpec :: forall edit sela selb. UISpec sela edit -> UISpec selb edit
noSelectionUISpec spec = MkUISpec $ MkNoSelectionUISpec spec
