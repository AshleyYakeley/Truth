module Truth.Core.UI.Specifier.CSS where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UIName sel edit where
    MkUIName :: Text -> UISpec sel edit -> UIName sel edit

instance Show (UIName sel edit) where
    show (MkUIName name spec) = "name " ++ show name ++ ": " ++ show spec

instance UIType UIName where
    uiWitness = $(iowitness [t|UIName|])

uiName :: Text -> UISpec sel edit -> UISpec sel edit
uiName name spec = MkUISpec $ MkUIName name spec

data UICSSClass sel edit where
    MkUICSSClass :: Text -> UISpec sel edit -> UICSSClass sel edit

instance Show (UICSSClass sel edit) where
    show (MkUICSSClass cssclass spec) = "css-class " ++ show cssclass ++ ": " ++ show spec

instance UIType UICSSClass where
    uiWitness = $(iowitness [t|UICSSClass|])

uiCSSClass :: Text -> UISpec sel edit -> UISpec sel edit
uiCSSClass cssclass spec = MkUISpec $ MkUICSSClass cssclass spec

data UICSSStyleSheet sel edit where
    MkUICSSStyleSheet :: Bool -> Word32 -> Text -> UISpec sel edit -> UICSSStyleSheet sel edit

instance Show (UICSSStyleSheet sel edit) where
    show (MkUICSSStyleSheet _ priority _ spec) = "css-stylesheet (" ++ show priority ++ ")" ++ show spec

instance UIType UICSSStyleSheet where
    uiWitness = $(iowitness [t|UICSSStyleSheet|])

uiCSSStyleSheet :: Bool -> Word32 -> Text -> UISpec sel edit -> UISpec sel edit
uiCSSStyleSheet full priority css spec = MkUISpec $ MkUICSSStyleSheet full priority css spec
