module Truth.Core.UI.Icon where

import Truth.Core.Import
import Truth.Core.UI.Specifier

data StockIcon =
    SiDnD

data StockSize
    = SizeDnD
    | SizeCustom Int

data UIIcon edit where
    MkUIIcon :: StockIcon -> StockSize -> UIIcon edit

instance Show (UIIcon edit) where
    show (MkUIIcon _ _) = "icon"

instance UIType UIIcon where
    uiWitness = $(iowitness [t|UIIcon|])

uiIcon :: StockIcon -> StockSize -> UISpec edit
uiIcon icon size = MkUISpec $ MkUIIcon icon size
