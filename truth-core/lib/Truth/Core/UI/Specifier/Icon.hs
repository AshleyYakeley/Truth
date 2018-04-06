module Truth.Core.UI.Specifier.Icon where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

-- | https://specifications.freedesktop.org/icon-naming-spec/icon-naming-spec-latest.html
type IconName = Text

data StockSize
    = SizeDnD
    | SizeCustom Int

data UIIcon edit where
    MkUIIcon :: IconName -> StockSize -> UIIcon edit

instance Show (UIIcon edit) where
    show (MkUIIcon _ _) = "icon"

instance UIType UIIcon where
    uiWitness = $(iowitness [t|UIIcon|])

uiIcon :: IconName -> StockSize -> UISpec edit
uiIcon icon size = MkUISpec $ MkUIIcon icon size