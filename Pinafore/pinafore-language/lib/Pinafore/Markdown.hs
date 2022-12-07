module Pinafore.Markdown where

import Shapes

-- | requires @pymdownx.escapeall@ extension
newtype Markdown = RawMarkdown
    { getRawMarkdown :: Text
    } deriving (Eq, Semigroup, Monoid, IsString)

instance Show Markdown where
    show (RawMarkdown t) = show t

escapeChars :: [Char] -> Text -> Text
escapeChars badchars t = let
    escapeChar :: Char -> String
    escapeChar c =
        if elem c badchars
            then ['\\', c]
            else [c]
    in pack $ mconcat $ fmap escapeChar $ unpack t

plainMarkdown :: Text -> Markdown
plainMarkdown t = RawMarkdown $ escapeChars "\\+*<>_`#.-[]" t

rawMarkdown :: Text -> Markdown
rawMarkdown = RawMarkdown

boldMarkdown :: Markdown -> Markdown
boldMarkdown (RawMarkdown m) = RawMarkdown $ "**" <> m <> "**"

italicMarkdown :: Markdown -> Markdown
italicMarkdown (RawMarkdown m) = RawMarkdown $ "_" <> m <> "_"

codeMarkdown :: Text -> Markdown
codeMarkdown t = RawMarkdown $ "`" <> escapeChars "`" t <> "`"

titleMarkdown :: Int -> Markdown -> Markdown
titleMarkdown n m = RawMarkdown (pack $ replicate n '#') <> " " <> m

indentMarkdown :: Markdown -> Markdown
indentMarkdown (RawMarkdown m) = RawMarkdown $ intercalate "\n" $ fmap (\l -> "> " <> l) $ splitElem '\n' m

indentMarkdownN :: Int -> Markdown -> Markdown
indentMarkdownN 0 m = m
indentMarkdownN n m = indentMarkdown $ indentMarkdownN (pred n) m
