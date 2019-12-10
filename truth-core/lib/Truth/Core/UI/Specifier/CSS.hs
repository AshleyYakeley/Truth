module Truth.Core.UI.Specifier.CSS where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data NameUISpec sel update where
    MkNameUISpec :: Text -> UISpec sel update -> NameUISpec sel update

instance Show (NameUISpec sel update) where
    show (MkNameUISpec name spec) = "name " ++ show name ++ ": " ++ show spec

instance UIType NameUISpec where
    uiWitness = $(iowitness [t|NameUISpec|])

nameUISpec :: Text -> UISpec sel update -> UISpec sel update
nameUISpec name spec = MkUISpec $ MkNameUISpec name spec

data CSSClassUISpec sel update where
    MkCSSClassUISpec :: Text -> UISpec sel update -> CSSClassUISpec sel update

instance Show (CSSClassUISpec sel update) where
    show (MkCSSClassUISpec cssclass spec) = "css-class " ++ show cssclass ++ ": " ++ show spec

instance UIType CSSClassUISpec where
    uiWitness = $(iowitness [t|CSSClassUISpec|])

cssClassUISpec :: Text -> UISpec sel update -> UISpec sel update
cssClassUISpec cssclass spec = MkUISpec $ MkCSSClassUISpec cssclass spec

data CSSStyleSheetUISpec sel update where
    MkCSSStyleSheetUISpec :: Bool -> Word32 -> Text -> UISpec sel update -> CSSStyleSheetUISpec sel update

instance Show (CSSStyleSheetUISpec sel update) where
    show (MkCSSStyleSheetUISpec _ priority _ spec) = "css-stylesheet (" ++ show priority ++ ")" ++ show spec

instance UIType CSSStyleSheetUISpec where
    uiWitness = $(iowitness [t|CSSStyleSheetUISpec|])

cssStyleSheetUISpec :: Bool -> Word32 -> Text -> UISpec sel update -> UISpec sel update
cssStyleSheetUISpec full priority css spec = MkUISpec $ MkCSSStyleSheetUISpec full priority css spec
