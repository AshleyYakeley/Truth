module Truth.Core.UI.Specifier.Icon where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

-- | https://specifications.freedesktop.org/icon-naming-spec/icon-naming-spec-latest.html
type IconName = Text

data StockSize
    = SizeDnD
    | SizeCustom Int

data IconUISpec sel where
    MkIconUISpec :: IconName -> StockSize -> IconUISpec sel

instance Show (IconUISpec sel) where
    show (MkIconUISpec _ _) = "icon"

instance UIType IconUISpec where
    uiWitness = $(iowitness [t|IconUISpec|])

iconUISpec :: IconName -> StockSize -> LUISpec sel
iconUISpec icon size = mkLUISpec $ MkIconUISpec icon size
