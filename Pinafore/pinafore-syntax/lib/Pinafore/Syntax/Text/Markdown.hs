module Pinafore.Syntax.Text.Markdown
    ( PlainText(..)
    , RawMarkdown(..)
    , asRawMarkdown
    , MarkdownText
    , rawMarkdown
    , tagMarkdown
    , boldMarkdown
    , italicMarkdown
    , codeMarkdown
    , Markdown
    , paragraphMarkdown
    , titleMarkdown
    , indentMarkdown
    , indentMarkdownN
    ) where

import Pinafore.Syntax.Text.ToText
import Shapes

class PlainText t where
    plainText :: Text -> t

instance PlainText Text where
    plainText t = t

instance PlainText String where
    plainText = unpack

newtype RawMarkdown =
    MkRawMarkdown Text
    deriving newtype (Eq, Semigroup, Monoid, Show, IsString, ToText)

instance PlainText RawMarkdown where
    plainText t = MkRawMarkdown $ toText $ (plainText t :: MarkdownText)

asRawMarkdown :: Markdown -> RawMarkdown
asRawMarkdown m = MkRawMarkdown $ toText m

data MItem
    = RawMI RawMarkdown
    | PlainMI Text
    | CodeMI Text
    | BoldMI MarkdownText
    | ItalicMI MarkdownText
    | TagMI Text
            [(Text, Text)]
            MarkdownText
    deriving stock (Eq)

instance PlainText MItem where
    plainText = PlainMI

joinMItems :: MItem -> MItem -> Maybe MItem
joinMItems (RawMI a) (RawMI b) = Just $ RawMI $ a <> b
joinMItems (PlainMI a) (PlainMI b) = Just $ PlainMI $ a <> b
joinMItems (CodeMI a) (CodeMI b) = Just $ CodeMI $ a <> b
joinMItems (BoldMI a) (BoldMI b) = Just $ BoldMI $ a <> b
joinMItems (ItalicMI a) (ItalicMI b) = Just $ ItalicMI $ a <> b
joinMItems _ _ = Nothing

newtype MarkdownText =
    MkMarkdownText [MItem]
    deriving newtype (Eq)

instance Semigroup MarkdownText where
    MkMarkdownText aa <> MkMarkdownText bb =
        MkMarkdownText $
        fromMaybe (aa <> bb) $ do
            naa <- nonEmpty aa
            nbb <- nonEmpty bb
            joined <- joinMItems (last naa) (head nbb)
            return $ init naa <> [joined] <> tail nbb

instance Monoid MarkdownText where
    mempty = MkMarkdownText mempty

instance IsString MarkdownText where
    fromString s = plainText $ fromString s

instance Show MarkdownText where
    show m = show $ toText m

mItem :: MItem -> MarkdownText
mItem = MkMarkdownText . pure

escapeChars :: Text -> Text
escapeChars t = let
    escapeChar :: Char -> String
    escapeChar '\n' = "  \n"
    escapeChar c =
        if elem c ("\\+*&<>_`#.-[]|" :: [Char])
            then ['\\', c]
            else [c]
    in pack $ concatmap escapeChar $ unpack t

instance ToText MItem where
    toText (RawMI t) = toText t
    toText (PlainMI t) = escapeChars t
    toText (CodeMI t) = "<code>" <> escapeChars t <> "</code>"
    toText (BoldMI m) = "**" <> toText m <> "**"
    toText (ItalicMI m) = "_" <> toText m <> "_"
    toText (TagMI tagname params m) =
        "<" <>
        tagname <>
        concatmap (\(n, t) -> " " <> n <> "=" <> (pack $ show $ unpack t)) params <>
        ">" <> toText m <> "</" <> tagname <> ">"

instance ToText MarkdownText where
    toText (MkMarkdownText items) = toText items

paragraphMarkdown :: MarkdownText -> Markdown
paragraphMarkdown p = MkMarkdown [ParagraphBlock p]

rawMarkdown :: RawMarkdown -> MarkdownText
rawMarkdown = mItem . RawMI

instance PlainText MarkdownText where
    plainText = mItem . plainText

tagMarkdown :: Text -> [(Text, Text)] -> MarkdownText -> MarkdownText
tagMarkdown tagname params m = mItem $ TagMI tagname params m

boldMarkdown :: MarkdownText -> MarkdownText
boldMarkdown = mItem . BoldMI

italicMarkdown :: MarkdownText -> MarkdownText
italicMarkdown = mItem . ItalicMI

codeMI :: MItem -> MItem
codeMI (RawMI t) = CodeMI $ toText t
codeMI (PlainMI t) = CodeMI t
codeMI (CodeMI t) = CodeMI t
codeMI (BoldMI m) = BoldMI $ codeMarkdown m
codeMI (ItalicMI m) = ItalicMI $ codeMarkdown m
codeMI (TagMI tag pp m) = TagMI tag pp $ codeMarkdown m

codeMarkdown :: MarkdownText -> MarkdownText
codeMarkdown (MkMarkdownText m) = MkMarkdownText $ fmap codeMI m

data Block
    = ParagraphBlock MarkdownText
    | IndentBlock Markdown
    | TitleBlock Int
                 MarkdownText
    deriving stock (Eq)

block :: Block -> Markdown
block = MkMarkdown . pure

dropLastNewline :: String -> String
dropLastNewline "" = ""
dropLastNewline "\n" = ""
dropLastNewline (c:cc) = c : dropLastNewline cc

blockToText :: String -> Block -> Text
blockToText indent (ParagraphBlock t) =
    pack $ indent <> replaceListAll "\n\n" ("\n" <> indent <> "\n") (dropLastNewline (unpack $ toText t) <> "\n\n")
blockToText indent (IndentBlock (MkMarkdown blocks)) = let
    newindent = indent <> "> "
    in concatmap (blockToText newindent) blocks
blockToText indent (TitleBlock n m) =
    (pack $ indent <> replicate n '#' <> " ") <> toText m <> (pack $ "\n" <> indent <> "\n")

instance ToText Block where
    toText = blockToText ""

instance ToText Markdown where
    toText (MkMarkdown items) = toText items

-- | CommonMark
newtype Markdown =
    MkMarkdown [Block]
    deriving newtype (Eq, Semigroup, Monoid)

instance PlainText Markdown where
    plainText = paragraphMarkdown . plainText

instance IsString Markdown where
    fromString s = plainText $ fromString s

instance Show Markdown where
    show m = show $ toText m

titleMarkdown :: Int -> MarkdownText -> Markdown
titleMarkdown n m = block $ TitleBlock n m

indentMarkdown :: Markdown -> Markdown
indentMarkdown = block . IndentBlock

indentMarkdownN :: Int -> Markdown -> Markdown
indentMarkdownN 0 m = m
indentMarkdownN n m = indentMarkdown $ indentMarkdownN (pred n) m
