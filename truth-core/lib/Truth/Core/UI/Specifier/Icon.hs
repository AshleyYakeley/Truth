module Truth.Core.UI.Specifier.Icon where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

-- | https://specifications.freedesktop.org/icon-naming-spec/icon-naming-spec-latest.html
type IconName = Text

data StockSize
    = SizeDnD
    | SizeCustom Int

data IconUISpec sel edit where
    MkIconUISpec :: IconName -> StockSize -> IconUISpec sel edit

instance Show (IconUISpec sel edit) where
    show (MkIconUISpec _ _) = "icon"

instance UIType IconUISpec where
    uiWitness = $(iowitness [t|IconUISpec|])

iconUISpec :: IconName -> StockSize -> UISpec sel edit
iconUISpec icon size = MkUISpec $ MkIconUISpec icon size
