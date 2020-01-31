module Truth.Core.UI.Specifier.CSS where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data NameUISpec sel where
    MkNameUISpec :: Text -> LUISpec sel -> NameUISpec sel

instance Show (NameUISpec sel) where
    show (MkNameUISpec name _) = "name " ++ show name

instance UIType NameUISpec where
    uiWitness = $(iowitness [t|NameUISpec|])

nameUISpec :: Text -> LUISpec sel -> LUISpec sel
nameUISpec name spec = mkLUISpec $ MkNameUISpec name spec

data CSSClassUISpec sel where
    MkCSSClassUISpec :: Text -> LUISpec sel -> CSSClassUISpec sel

instance Show (CSSClassUISpec sel) where
    show (MkCSSClassUISpec cssclass _) = "css-class " ++ show cssclass

instance UIType CSSClassUISpec where
    uiWitness = $(iowitness [t|CSSClassUISpec|])

cssClassUISpec :: Text -> LUISpec sel -> LUISpec sel
cssClassUISpec cssclass spec = mkLUISpec $ MkCSSClassUISpec cssclass spec

data CSSStyleSheetUISpec sel where
    MkCSSStyleSheetUISpec :: Bool -> Word32 -> Text -> LUISpec sel -> CSSStyleSheetUISpec sel

instance Show (CSSStyleSheetUISpec sel) where
    show (MkCSSStyleSheetUISpec _ priority _ _) = "css-stylesheet (" ++ show priority ++ ")"

instance UIType CSSStyleSheetUISpec where
    uiWitness = $(iowitness [t|CSSStyleSheetUISpec|])

cssStyleSheetUISpec :: Bool -> Word32 -> Text -> LUISpec sel -> LUISpec sel
cssStyleSheetUISpec full priority css spec = mkLUISpec $ MkCSSStyleSheetUISpec full priority css spec
