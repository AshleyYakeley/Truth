module Truth.Core.UI.Specifier.CSS where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data NameUISpec sel where
    MkNameUISpec :: Text -> UISpec sel -> NameUISpec sel

instance Show (NameUISpec sel) where
    show (MkNameUISpec name spec) = "name " ++ show name ++ ": " ++ show spec

instance UIType NameUISpec where
    uiWitness = $(iowitness [t|NameUISpec|])

nameUISpec :: Text -> UISpec sel -> UISpec sel
nameUISpec name spec = MkUISpec $ MkNameUISpec name spec

data CSSClassUISpec sel where
    MkCSSClassUISpec :: Text -> UISpec sel -> CSSClassUISpec sel

instance Show (CSSClassUISpec sel) where
    show (MkCSSClassUISpec cssclass spec) = "css-class " ++ show cssclass ++ ": " ++ show spec

instance UIType CSSClassUISpec where
    uiWitness = $(iowitness [t|CSSClassUISpec|])

cssClassUISpec :: Text -> UISpec sel -> UISpec sel
cssClassUISpec cssclass spec = MkUISpec $ MkCSSClassUISpec cssclass spec

data CSSStyleSheetUISpec sel where
    MkCSSStyleSheetUISpec :: Bool -> Word32 -> Text -> UISpec sel -> CSSStyleSheetUISpec sel

instance Show (CSSStyleSheetUISpec sel) where
    show (MkCSSStyleSheetUISpec _ priority _ spec) = "css-stylesheet (" ++ show priority ++ ")" ++ show spec

instance UIType CSSStyleSheetUISpec where
    uiWitness = $(iowitness [t|CSSStyleSheetUISpec|])

cssStyleSheetUISpec :: Bool -> Word32 -> Text -> UISpec sel -> UISpec sel
cssStyleSheetUISpec full priority css spec = MkUISpec $ MkCSSStyleSheetUISpec full priority css spec
