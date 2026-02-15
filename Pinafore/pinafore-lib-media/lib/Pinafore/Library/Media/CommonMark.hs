module Pinafore.Library.Media.CommonMark
    ( CommonMarkText (..)
    , commonMarkStuff
    )
where

import Commonmark qualified as C
import Commonmark.Extensions qualified as C
import Data.Shim
import Pinafore.API
import Shapes

import Pinafore.Library.Media.HTML
import Pinafore.Library.Media.Media

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
toHTML (MkCommonMarkText t) =
    mapResultFailure parseErrorToLS $ do
        let
            -- https://github.com/jgm/commonmark-hs/tree/master/commonmark-extensions
            customSyntax :: C.SyntaxSpec Identity (C.Html ()) (C.Html ())
            customSyntax =
                mconcat
                    [ mif False C.hardLineBreaksSpec
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
        html :: C.Html () <- eitherToResult $ runIdentity $ C.commonmarkWith customSyntax "" t
        return $ MkHTMLText $ toStrict $ C.renderHtml html

commonMarkStuff :: LibraryStuff
commonMarkStuff =
    headingBDS "CommonMark" ""
        $ [ typeBDS
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
                   ]
          ]
