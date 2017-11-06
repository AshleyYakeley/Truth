module Truth.Core.UI.Layout where

import Truth.Core.Import
import Truth.Core.UI.Specifier

data UIVertical edit where
    MkUIVertical :: [UISpec edit] -> UIVertical edit

instance Show (UIVertical edit) where
    show (MkUIVertical specs) = "vertical (" ++ intercalate ", " (fmap show specs) ++ ")"

instance UIType UIVertical where
    uiWitness = $(iowitness [t|UIVertical|])

uiVertical :: [UISpec edit] -> UISpec edit
uiVertical specs = MkUISpec $ MkUIVertical specs
