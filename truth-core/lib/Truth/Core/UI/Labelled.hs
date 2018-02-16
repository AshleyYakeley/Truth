module Truth.Core.UI.Labelled where

import Truth.Core.Import
import Truth.Core.UI.Specifier

data UILabelled edit where
    MkUILabelled :: Text -> UISpec edit -> UILabelled edit

instance Show (UILabelled edit) where
    show (MkUILabelled l s) = "labelled " ++ show l ++ " " ++ show s

instance UIType UILabelled where
    uiWitness = $(iowitness [t|UILabelled|])

uiLabelled :: Text -> UISpec edit -> UISpec edit
uiLabelled l s = MkUISpec $ MkUILabelled l s
