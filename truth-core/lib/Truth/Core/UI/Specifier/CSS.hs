module Truth.Core.UI.Specifier.CSS where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UIName edit where
    MkUIName :: Text -> UISpec edit -> UIName edit

instance Show (UIName edit) where
    show (MkUIName name spec) = "name " ++ show name ++ ": " ++ show spec

instance UIType UIName where
    uiWitness = $(iowitness [t|UIName|])

uiName :: Text -> UISpec edit -> UISpec edit
uiName name spec = MkUISpec $ MkUIName name spec

data UICSSClass edit where
    MkUICSSClass :: Text -> UISpec edit -> UICSSClass edit

instance Show (UICSSClass edit) where
    show (MkUICSSClass cssclass spec) = "css-class " ++ show cssclass ++ ": " ++ show spec

instance UIType UICSSClass where
    uiWitness = $(iowitness [t|UICSSClass|])

uiCSSClass :: Text -> UISpec edit -> UISpec edit
uiCSSClass cssclass spec = MkUISpec $ MkUICSSClass cssclass spec

data UICSSStyleSheet edit where
    MkUICSSStyleSheet :: Bool -> Word32 -> Text -> UISpec edit -> UICSSStyleSheet edit

instance Show (UICSSStyleSheet edit) where
    show (MkUICSSStyleSheet _ priority _ spec) = "css-stylesheet (" ++ show priority ++ ")" ++ show spec

instance UIType UICSSStyleSheet where
    uiWitness = $(iowitness [t|UICSSStyleSheet|])

uiCSSStyleSheet :: Bool -> Word32 -> Text -> UISpec edit -> UISpec edit
uiCSSStyleSheet full priority css spec = MkUISpec $ MkUICSSStyleSheet full priority css spec
