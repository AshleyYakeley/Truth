module Truth.Core.UI.TextStyle where

import Truth.Core.Import

data TextStyle = MkTextStyle
    { tsItalic :: Bool
    } deriving (Eq)

plainTextStyle :: TextStyle
plainTextStyle = let
    tsItalic = False
    in MkTextStyle {..}
