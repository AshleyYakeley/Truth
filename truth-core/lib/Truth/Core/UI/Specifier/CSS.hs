module Truth.Core.UI.Specifier.CSS where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UIName seledit edit where
    MkUIName :: Text -> UISpec seledit edit -> UIName seledit edit

instance Show (UIName seledit edit) where
    show (MkUIName name spec) = "name " ++ show name ++ ": " ++ show spec

instance UIType UIName where
    uiWitness = $(iowitness [t|UIName|])

uiName :: Text -> UISpec seledit edit -> UISpec seledit edit
uiName name spec = MkUISpec $ MkUIName name spec

data UICSSClass seledit edit where
    MkUICSSClass :: Text -> UISpec seledit edit -> UICSSClass seledit edit

instance Show (UICSSClass seledit edit) where
    show (MkUICSSClass cssclass spec) = "css-class " ++ show cssclass ++ ": " ++ show spec

instance UIType UICSSClass where
    uiWitness = $(iowitness [t|UICSSClass|])

uiCSSClass :: Text -> UISpec seledit edit -> UISpec seledit edit
uiCSSClass cssclass spec = MkUISpec $ MkUICSSClass cssclass spec

data UICSSStyleSheet seledit edit where
    MkUICSSStyleSheet :: Bool -> Word32 -> Text -> UISpec seledit edit -> UICSSStyleSheet seledit edit

instance Show (UICSSStyleSheet seledit edit) where
    show (MkUICSSStyleSheet _ priority _ spec) = "css-stylesheet (" ++ show priority ++ ")" ++ show spec

instance UIType UICSSStyleSheet where
    uiWitness = $(iowitness [t|UICSSStyleSheet|])

uiCSSStyleSheet :: Bool -> Word32 -> Text -> UISpec seledit edit -> UISpec seledit edit
uiCSSStyleSheet full priority css spec = MkUISpec $ MkUICSSStyleSheet full priority css spec
