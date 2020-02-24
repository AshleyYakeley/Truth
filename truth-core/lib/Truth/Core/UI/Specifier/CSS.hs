module Truth.Core.UI.Specifier.CSS where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data NameUISpec where
    MkNameUISpec :: Text -> CVUISpec -> NameUISpec

instance Show NameUISpec where
    show (MkNameUISpec name _) = "name " ++ show name

instance UIType NameUISpec where
    uiWitness = $(iowitness [t|NameUISpec|])

nameUISpec :: Text -> CVUISpec -> CVUISpec
nameUISpec name spec = mkCVUISpec $ MkNameUISpec name spec

data CSSClassUISpec where
    MkCSSClassUISpec :: Text -> CVUISpec -> CSSClassUISpec

instance Show CSSClassUISpec where
    show (MkCSSClassUISpec cssclass _) = "css-class " ++ show cssclass

instance UIType CSSClassUISpec where
    uiWitness = $(iowitness [t|CSSClassUISpec|])

cssClassUISpec :: Text -> CVUISpec -> CVUISpec
cssClassUISpec cssclass spec = mkCVUISpec $ MkCSSClassUISpec cssclass spec

data CSSStyleSheetUISpec where
    MkCSSStyleSheetUISpec :: Bool -> Word32 -> Text -> CVUISpec -> CSSStyleSheetUISpec

instance Show CSSStyleSheetUISpec where
    show (MkCSSStyleSheetUISpec _ priority _ _) = "css-stylesheet (" ++ show priority ++ ")"

instance UIType CSSStyleSheetUISpec where
    uiWitness = $(iowitness [t|CSSStyleSheetUISpec|])

cssStyleSheetUISpec :: Bool -> Word32 -> Text -> CVUISpec -> CVUISpec
cssStyleSheetUISpec full priority css spec = mkCVUISpec $ MkCSSStyleSheetUISpec full priority css spec
