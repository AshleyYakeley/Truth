module Truth.Core.UI.Specifier.SelectionLens where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UISetSelectionMap sel edit where
    MkUISetSelectionMap :: forall sela selb edit. (sela -> selb) -> UISpec sela edit -> UISetSelectionMap selb edit

instance Show (UISetSelectionMap sel edit) where
    show (MkUISetSelectionMap _ uispec) = "selection-lens " ++ show uispec

instance UIType UISetSelectionMap where
    uiWitness = $(iowitness [t|UISetSelectionMap|])

uiSetSelectionMap :: forall sela selb edit. (sela -> selb) -> UISpec sela edit -> UISpec selb edit
uiSetSelectionMap f spec = MkUISpec $ MkUISetSelectionMap f spec

aspectIOMapSelection :: IO (sela -> selb) -> Aspect sela -> Aspect selb
aspectIOMapSelection iof aspect = do
    f <- iof
    msel <- aspect
    return $ do
        sel <- msel
        return $ f sel

aspectMapSelection :: (sela -> selb) -> Aspect sela -> Aspect selb
aspectMapSelection f = aspectIOMapSelection $ return f

data UINoSelection sel edit where
    MkUINoSelection :: UISpec sela edit -> UINoSelection selb edit

instance Show (UINoSelection sel edit) where
    show (MkUINoSelection uispec) = "no-selection-lens " ++ show uispec

instance UIType UINoSelection where
    uiWitness = $(iowitness [t|UINoSelection|])

uiNoSelectionLens :: forall edit sela selb. UISpec sela edit -> UISpec selb edit
uiNoSelectionLens spec = MkUISpec $ MkUINoSelection spec
