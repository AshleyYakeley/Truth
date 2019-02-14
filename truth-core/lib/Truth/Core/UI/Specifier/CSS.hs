module Truth.Core.UI.Specifier.CSS where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data NameUISpec sel edit where
    MkNameUISpec :: Text -> UISpec sel edit -> NameUISpec sel edit

instance Show (NameUISpec sel edit) where
    show (MkNameUISpec name spec) = "name " ++ show name ++ ": " ++ show spec

instance UIType NameUISpec where
    uiWitness = $(iowitness [t|NameUISpec|])

nameUISpec :: Text -> UISpec sel edit -> UISpec sel edit
nameUISpec name spec = MkUISpec $ MkNameUISpec name spec

data CSSClassUISpec sel edit where
    MkCSSClassUISpec :: Text -> UISpec sel edit -> CSSClassUISpec sel edit

instance Show (CSSClassUISpec sel edit) where
    show (MkCSSClassUISpec cssclass spec) = "css-class " ++ show cssclass ++ ": " ++ show spec

instance UIType CSSClassUISpec where
    uiWitness = $(iowitness [t|CSSClassUISpec|])

cssClassUISpec :: Text -> UISpec sel edit -> UISpec sel edit
cssClassUISpec cssclass spec = MkUISpec $ MkCSSClassUISpec cssclass spec

data CSSStyleSheetUISpec sel edit where
    MkCSSStyleSheetUISpec :: Bool -> Word32 -> Text -> UISpec sel edit -> CSSStyleSheetUISpec sel edit

instance Show (CSSStyleSheetUISpec sel edit) where
    show (MkCSSStyleSheetUISpec _ priority _ spec) = "css-stylesheet (" ++ show priority ++ ")" ++ show spec

instance UIType CSSStyleSheetUISpec where
    uiWitness = $(iowitness [t|CSSStyleSheetUISpec|])

cssStyleSheetUISpec :: Bool -> Word32 -> Text -> UISpec sel edit -> UISpec sel edit
cssStyleSheetUISpec full priority css spec = MkUISpec $ MkCSSStyleSheetUISpec full priority css spec
