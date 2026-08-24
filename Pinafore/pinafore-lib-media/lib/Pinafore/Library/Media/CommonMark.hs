{-# OPTIONS -fno-warn-orphans #-}
module Pinafore.Library.Media.CommonMark
    ( CommonMarkText (..)
    , commonMarkStuff
    )
where

import Commonmark qualified as C
import Commonmark.Extensions qualified as C
import Commonmark.Inlines qualified as C
import Data.Shim
import Pinafore.API
import Shapes
import Text.Parsec qualified as P
import Text.Parsec.Pos qualified as P

import Pinafore.Library.Media.HTML
import Pinafore.Library.Media.Media

instance
    {-# OVERLAPPABLE #-}
    (Functor f, C.HasAttributes a) =>
    C.HasAttributes (f a)
    where
    addAttributes attr = fmap $ C.addAttributes attr

instance
    {-# OVERLAPPABLE #-}
    (Functor f, C.Rangeable b) =>
    C.Rangeable (f b)
    where
    ranged sr = fmap $ C.ranged sr

instance
    {-# OVERLAPPABLE #-}
    (Applicative f, Show (f il), Monoid (f il), C.IsInline il) =>
    C.IsInline (f il)
    where
    lineBreak = pure C.lineBreak
    softBreak = pure C.softBreak
    str s = pure $ C.str s
    entity s = pure $ C.entity s
    escapedChar s = pure $ C.escapedChar s
    emph = fmap C.emph
    strong = fmap C.strong
    link d t = fmap $ C.link d t
    image s t = fmap $ C.image s t
    code s = pure $ C.code s
    rawInline f t = pure $ C.rawInline f t

instance
    {-# OVERLAPPABLE #-}
    (Applicative f, Monoid (f il), Monoid (f b), Show (f il), Show (f b), C.IsBlock il b) =>
    C.IsBlock (f il) (f b)
    where
    paragraph = fmap C.paragraph
    plain = fmap C.plain
    thematicBreak = pure C.thematicBreak
    blockQuote = fmap C.blockQuote
    codeBlock info content = pure $ C.codeBlock info content
    heading level = fmap $ C.heading level
    rawBlock format content = pure $ C.rawBlock format content
    referenceLinkDefinition label destination = pure $ C.referenceLinkDefinition label destination
    list listType spacing items = C.list listType spacing <$> sequenceA items

instance
    {-# OVERLAPPABLE #-}
    (Applicative f, Monoid (f a), Show (f a), C.HasQuoted a) =>
    C.HasQuoted (f a)
    where
    singleQuoted = fmap C.singleQuoted
    doubleQuoted = fmap C.doubleQuoted

instance
    {-# OVERLAPPABLE #-}
    (Functor f, C.HasStrikethrough a) =>
    C.HasStrikethrough (f a)
    where
    strikethrough = fmap C.strikethrough

instance
    {-# OVERLAPPABLE #-}
    (Functor f, C.HasSuperscript a) =>
    C.HasSuperscript (f a)
    where
    superscript = fmap C.superscript

instance
    {-# OVERLAPPABLE #-}
    (Functor f, C.HasSubscript a) =>
    C.HasSubscript (f a)
    where
    subscript = fmap C.subscript

instance
    {-# OVERLAPPABLE #-}
    (Applicative f, C.HasMath a) =>
    C.HasMath (f a)
    where
    inlineMath = pure . C.inlineMath
    displayMath = pure . C.displayMath

instance
    {-# OVERLAPPABLE #-}
    (Applicative f, C.HasEmoji a) =>
    C.HasEmoji (f a)
    where
    emoji name = pure . C.emoji name

instance
    {-# OVERLAPPABLE #-}
    (Applicative f, C.HasPipeTable il b) =>
    C.HasPipeTable (f il) (f b)
    where
    pipeTable alignments headers rows =
        C.pipeTable alignments <$> sequenceA headers <*> traverse sequenceA rows

instance
    {-# OVERLAPPABLE #-}
    (Applicative f, Monoid (f il), Monoid (f b), Show (f il), Show (f b), C.HasFootnote il b) =>
    C.HasFootnote (f il) (f b)
    where
    footnote number label = fmap $ C.footnote number label
    footnoteList notes = C.footnoteList <$> sequenceA notes
    footnoteRef label title = fmap $ C.footnoteRef label title

instance
    {-# OVERLAPPABLE #-}
    (Applicative f, Monoid (f il), Monoid (f b), Show (f il), Show (f b), C.HasDefinitionList il b) =>
    C.HasDefinitionList (f il) (f b)
    where
    definitionList spacing items =
        C.definitionList spacing
            <$> traverse (\(term, definitions) -> (,) <$> term <*> sequenceA definitions) items

instance
    {-# OVERLAPPABLE #-}
    (Applicative f, Monoid (f il), Monoid (f b), Show (f il), Show (f b), C.HasTaskList il b) =>
    C.HasTaskList (f il) (f b)
    where
    taskList listType spacing items =
        C.taskList listType spacing
            <$> traverse (\(checked, item) -> (,) checked <$> item) items

instance
    {-# OVERLAPPABLE #-}
    (Applicative f, Monoid (f a), Show (f a), C.HasSpan a) =>
    C.HasSpan (f a)
    where
    spanWith attributes = fmap $ C.spanWith attributes

instance
    {-# OVERLAPPABLE #-}
    (Functor f, C.HasDiv a) =>
    C.HasDiv (f a)
    where
    div_ = fmap C.div_

instance
    {-# OVERLAPPABLE #-}
    (Functor f, C.HasWikilinks a) =>
    C.HasWikilinks (f a)
    where
    wikilink target = fmap $ C.wikilink target

instance
    {-# OVERLAPPABLE #-}
    (Applicative f, Monoid (f il), Monoid (f b), Show (f il), Show (f b), C.HasAlerts il b) =>
    C.HasAlerts (f il) (f b)
    where
    alert alertType = fmap $ C.alert alertType

inlineSyntaxSpec :: C.InlineParser m il -> C.SyntaxSpec m il b
inlineSyntaxSpec spec = mempty{C.syntaxInlineParsers = [spec]}

instance Show (ImmutableWholeModel (C.Html ())) where
    show _ = "<HTML model>"

newtype CommonMarkText = MkCommonMarkText
    { unCommonMarkText :: Text
    }
    deriving newtype (Eq, Semigroup, Monoid, AsTypedLiteral)

instance AsLiteral CommonMarkText

-- CommonMarkText
commonMarkTextGroundType :: QGroundType '[] CommonMarkText
commonMarkTextGroundType =
    mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily CommonMarkText)|]) "CommonMarkText"

instance HasQGroundType '[] CommonMarkText where
    qGroundType = commonMarkTextGroundType

asText :: Codec Text CommonMarkText
asText = MkCodec (Just . MkCommonMarkText) unCommonMarkText

asMedia :: Codec Media CommonMarkText
asMedia =
    coerceCodec
        . mediaSpecificText
            (MkMediaType TextMediaType "markdown" [("variant", "CommonMark")])
            ( \case
                MkMediaType TextMediaType "markdown" _ -> True
                _ -> False
            )

parseErrorToLS :: C.ParseError -> Located Showable
parseErrorToLS err = fmap (MkShowable . toText . getMessagesNamedText) $ parseErrorMessage err

toHTML :: CommonMarkText -> Result (Located Showable) HTMLText
toHTML (MkCommonMarkText t) = do
    let
        -- https://github.com/jgm/commonmark-hs/tree/master/commonmark-extensions
        customSyntax :: C.SyntaxSpec (Result (Located Showable)) (C.Html ()) (C.Html ())
        customSyntax =
            mconcat
                [ C.defaultSyntaxSpec
                , mif False C.hardLineBreaksSpec
                , mif True C.smartPunctuationSpec
                , mif True C.strikethroughSpec
                , mif True C.superscriptSpec
                , mif True C.subscriptSpec
                , mif True C.mathSpec
                , mif True C.emojiSpec
                , mif False C.autolinkSpec
                , mif True C.pipeTableSpec
                , mif True C.footnoteSpec
                , mif True C.definitionListSpec
                , mif True C.fancyListSpec
                , mif False C.taskListSpec
                , mif True C.attributesSpec
                , mif True C.rawAttributeSpec
                , mif True C.bracketedSpanSpec
                , mif True C.fencedDivSpec
                , mif True C.autoIdentifiersSpec
                , mif False C.autoIdentifiersAsciiSpec
                , mif True C.implicitHeadingReferencesSpec
                , mif False $ C.wikilinksSpec C.TitleBeforePipe
                , mif True C.alertSpec
                , mif False C.rebaseRelativePathsSpec
                ]
    ehtml <- C.commonmarkWith customSyntax "" t
    html :: C.Html () <- mapResultFailure parseErrorToLS $ eitherToResult ehtml
    return $ MkHTMLText $ toStrict $ C.renderHtml html

parseToken :: forall s m a. Monad m => (C.Tok -> Maybe a) -> P.ParsecT [C.Tok] s m a
parseToken matcher = let
    updatePos :: P.SourcePos -> C.Tok -> [C.Tok] -> P.SourcePos
    updatePos _spos _ (C.Tok _ pos _ : _) = pos
    updatePos !spos (C.Tok _ _pos t) [] =
        P.updatePosString spos (unpack t)
    in P.tokenPrim (unpack . C.tokContents) updatePos matcher

symbol :: forall s m. Monad m => Char -> P.ParsecT [C.Tok] s m ()
symbol ec = parseToken $ \case
    C.Tok (C.Symbol fc) _ _ | fc == ec -> Just ()
    _ -> Nothing

modelInlineSpec :: C.InlineParser (Result (Located Showable)) (ImmutableWholeModel (C.Html ()))
modelInlineSpec = do
    symbol '@'
    symbol '`'
    texts <- P.many $ parseToken $ \case
        C.Tok (C.Symbol '`') _ _ -> Nothing
        C.Tok _ _ t -> Just t
    symbol '`'
    return $ pure $ C.htmlText $ mconcat texts

toHTMLModel :: CommonMarkText -> Result (Located Showable) (ImmutableWholeModel HTMLText)
toHTMLModel (MkCommonMarkText t) = do
    let
        customSyntax :: C.SyntaxSpec (Result (Located Showable)) (ImmutableWholeModel (C.Html ())) (ImmutableWholeModel (C.Html ()))
        customSyntax =
            mconcat
                [ C.defaultSyntaxSpec
                , mif False C.hardLineBreaksSpec
                , mif True C.smartPunctuationSpec
                , mif True C.strikethroughSpec
                , mif True C.superscriptSpec
                , mif True C.subscriptSpec
                , mif True C.mathSpec
                , mif True C.emojiSpec
                , mif False C.autolinkSpec
                , mif True C.pipeTableSpec
                , mif True C.footnoteSpec
                , mif True C.definitionListSpec
                , mif True C.fancyListSpec
                , mif False C.taskListSpec
                , mif True C.attributesSpec
                , mif True C.rawAttributeSpec
                , mif True C.bracketedSpanSpec
                , mif True C.fencedDivSpec
                , mif True C.implicitHeadingReferencesSpec
                , mif False $ C.wikilinksSpec C.TitleBeforePipe
                , mif True C.alertSpec
                , mif False C.rebaseRelativePathsSpec
                , inlineSyntaxSpec modelInlineSpec
                ]
    emh <- C.commonmarkWith customSyntax "" t
    htmlModel :: ImmutableWholeModel (C.Html ()) <- mapResultFailure parseErrorToLS $ eitherToResult emh
    return $ fmap (MkHTMLText . toStrict . C.renderHtml) htmlModel

commonMarkStuff :: LibraryStuff
commonMarkStuff =
    headingBDS
        "CommonMark"
        ""
        [ typeBDS
            "CommonMarkText"
            "Text that's intended to be CommonMark (not necessarily valid)."
            (MkSomeGroundType commonMarkTextGroundType)
            [valPatBDS "Mk" "" MkCommonMarkText $ PureFunction $ pure $ \(MkCommonMarkText t) -> (t, ())]
        , hasSubtypeRelationBDS @CommonMarkText @Text Verify "" $ functionToShim "unCommonMarkText" unCommonMarkText
        , namespaceBDS "CommonMarkText"
            $ monoidEntries @CommonMarkText
            <> [ valBDS "asText" "" $ codecToPrism asText
               , valBDS "asMedia" "" $ codecToPrism asMedia
               , valBDS "toHTML" "render as HTML" toHTML
               , valBDS "toHTMLModel" "render as HTML with inserts, e.g. `@\\`ap{show %clock}\\``" toHTMLModel
               ]
        ]
