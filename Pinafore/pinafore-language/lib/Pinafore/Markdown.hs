module Pinafore.Markdown
    ( Markdown
    , getRawMarkdown
    , rawMarkdown
    , plainMarkdown
    , tagMarkdown
    , boldMarkdown
    , italicMarkdown
    , codeMarkdown
    , titleMarkdown
    , indentMarkdown
    , indentMarkdownN
    ) where

import Shapes

data MItem
    = RawMI Text
    | PlainMI Text
    | CodeMI Text
    | TitleMI Int
              Markdown
    | BoldMI Markdown
    | ItalicMI Markdown
    | IndentMI Markdown
    | TagMI Text
            [(Text, Text)]
            Markdown
    deriving (Eq)

joinMItems :: MItem -> MItem -> Maybe MItem
joinMItems (RawMI a) (RawMI b) = Just $ RawMI $ a <> b
joinMItems (PlainMI a) (PlainMI b) = Just $ PlainMI $ a <> b
joinMItems (CodeMI a) (CodeMI b) = Just $ CodeMI $ a <> b
joinMItems (BoldMI a) (BoldMI b) = Just $ BoldMI $ a <> b
joinMItems (ItalicMI a) (ItalicMI b) = Just $ ItalicMI $ a <> b
joinMItems (IndentMI a) (IndentMI b) = Just $ IndentMI $ a <> b
joinMItems _ _ = Nothing

-- | requires @pymdownx.escapeall@ extension
newtype Markdown =
    MkMarkdown [MItem]
    deriving (Eq)

instance Semigroup Markdown where
    MkMarkdown aa <> MkMarkdown bb =
        MkMarkdown $
        fromMaybe (aa <> bb) $ do
            naa <- nonEmpty aa
            nbb <- nonEmpty bb
            joined <- joinMItems (last naa) (head nbb)
            return $ init naa <> [joined] <> tail nbb

instance Monoid Markdown where
    mempty = MkMarkdown mempty

instance IsString Markdown where
    fromString s = plainMarkdown $ fromString s

instance Show Markdown where
    show m = show $ getRawMarkdown m

mItem :: MItem -> Markdown
mItem = MkMarkdown . pure

escapeChars :: [Char] -> Text -> Text
escapeChars badchars t = let
    escapeChar :: Char -> String
    escapeChar c =
        if elem c badchars
            then ['\\', c]
            else [c]
    in pack $ mconcat $ fmap escapeChar $ unpack t

getRawMI :: MItem -> Text
getRawMI (RawMI t) = t
getRawMI (PlainMI t) = escapeChars "\\+*<>_`#.-[]" t
getRawMI (CodeMI t) = "`" <> escapeChars "`" t <> "`"
getRawMI (TitleMI n m) = (pack $ replicate n '#') <> " " <> getRawMarkdown m
getRawMI (BoldMI m) = "**" <> getRawMarkdown m <> "**"
getRawMI (ItalicMI m) = "_" <> getRawMarkdown m <> "_"
getRawMI (IndentMI m) = intercalate "\n" $ fmap (\l -> "> " <> l) $ splitElem '\n' $ getRawMarkdown m
getRawMI (TagMI tagname params m) =
    "<" <>
    tagname <>
    mconcat (fmap (\(n, t) -> " " <> n <> "=" <> (pack $ show $ unpack t)) params) <>
    ">" <> getRawMarkdown m <> "</" <> tagname <> ">"

getRawMarkdown :: Markdown -> Text
getRawMarkdown (MkMarkdown items) = mconcat $ fmap getRawMI items

rawMarkdown :: Text -> Markdown
rawMarkdown = mItem . RawMI

plainMarkdown :: Text -> Markdown
plainMarkdown = mItem . PlainMI

tagMarkdown :: Text -> [(Text, Text)] -> Markdown -> Markdown
tagMarkdown tagname params m = mItem $ TagMI tagname params m

boldMarkdown :: Markdown -> Markdown
boldMarkdown = mItem . BoldMI

italicMarkdown :: Markdown -> Markdown
italicMarkdown = mItem . ItalicMI

codeMI :: MItem -> MItem
codeMI (RawMI t) = CodeMI t
codeMI (PlainMI t) = CodeMI t
codeMI (CodeMI t) = CodeMI t
codeMI (TitleMI i m) = TitleMI i $ codeMarkdown m
codeMI (BoldMI m) = BoldMI $ codeMarkdown m
codeMI (ItalicMI m) = ItalicMI $ codeMarkdown m
codeMI (IndentMI m) = IndentMI $ codeMarkdown m
codeMI (TagMI tag pp m) = TagMI tag pp $ codeMarkdown m

codeMarkdown :: Markdown -> Markdown
codeMarkdown (MkMarkdown m) = MkMarkdown $ fmap codeMI m

titleMarkdown :: Int -> Markdown -> Markdown
titleMarkdown n m = mItem $ TitleMI n m

indentMarkdown :: Markdown -> Markdown
indentMarkdown = mItem . IndentMI

indentMarkdownN :: Int -> Markdown -> Markdown
indentMarkdownN 0 m = m
indentMarkdownN n m = indentMarkdown $ indentMarkdownN (pred n) m
