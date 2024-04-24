module Pinafore.DocGen
    ( generateCommonMarkDoc
    ) where

import Pinafore.Documentation
import Pinafore.Language
import Pinafore.Main
import Shapes

isSubtypeRel :: Tree DefDoc -> Bool
isSubtypeRel (MkTree (MkDefDoc SubtypeRelationDocItem {} _) _) = True
isSubtypeRel _ = False

trimDocL :: Tree DefDoc -> Forest DefDoc
trimDocL (MkTree n children) =
    case (docItem n, trimDocChildren children) of
        (HeadingDocItem {}, children'@(MkForest tt))
            | all isSubtypeRel tt -> children'
        (_, children') -> pureForest $ MkTree n children'

trimDocChildren :: Forest DefDoc -> Forest DefDoc
trimDocChildren children = bindForest children trimDocL

trimDoc :: Tree DefDoc -> Tree DefDoc
trimDoc (MkTree n children) = MkTree n $ trimDocChildren children

generateCommonMarkDoc :: ModuleOptions -> Text -> IO ()
generateCommonMarkDoc modopts tmodname = do
    let fmodule = standardFetchModule modopts
    let ?library = mkLibraryContext nullInvocationInfo fmodule
    let modname = MkModuleName tmodname
    mmod <- fromInterpretResult $ runPinaforeScoped (unpack tmodname) $ lcLoadModule ?library modname
    pmodule <- maybeToM (unpack $ tmodname <> ": not found") mmod
    let
        runDocTree :: Int -> Int -> Tree DefDoc -> IO ()
        runDocTree hlevel ilevel (MkTree MkDefDoc {..} (MkForest children)) = do
            let
                putMarkdown :: Markdown -> IO ()
                putMarkdown m = hPutStr stdout $ unpack $ toText m
                putIndentMarkdown :: Markdown -> IO ()
                putIndentMarkdown m = putMarkdown $ indentMarkdownN ilevel m
                mapFullNameRef :: FullNameRef -> FullName
                mapFullNameRef fn = namespaceConcatFullName RootNamespace fn
                toMarkdown :: NamedText -> MarkdownText
                toMarkdown = plainText . toText
                showMarkdown ::
                       forall a. ShowNamedText a
                    => a
                    -> MarkdownText
                showMarkdown = toMarkdown . showNamedText
                trailingParams :: [NamedText] -> MarkdownText
                trailingParams pp = mconcat $ fmap (\p -> " " <> toMarkdown p) pp
                putBindDoc :: MarkdownText -> IO ()
                putBindDoc m = putIndentMarkdown $ paragraphMarkdown $ codeMarkdown m
                showNames :: NonEmpty FullNameRef -> MarkdownText
                showNames names = intercalate ", " $ toList $ fmap (boldMarkdown . showMarkdown . mapFullNameRef) names
            case docItem of
                ValueDocItem {..} ->
                    putBindDoc $ let
                        name = showNames diNames
                        nameType = name <> " : " <> toMarkdown diType
                        in nameType
                ValueSignatureDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ showMarkdown diSigName
                        nameType = name <> " : " <> toMarkdown diType
                        in nameType <>
                           if diHasDefault
                               then " [optional]"
                               else ""
                SupertypeConstructorSignatureDocItem {..} -> putBindDoc $ boldMarkdown $ showMarkdown diName
                ValuePatternDocItem {..} ->
                    putBindDoc $ let
                        name = showNames diNames
                        nameType = name <> " : " <> toMarkdown diType
                        in nameType
                SpecialFormDocItem {..} ->
                    putBindDoc $ let
                        name = showNames diNames
                        params = trailingParams diParams
                        nameType = name <> params <> ": " <> toMarkdown diType
                        in nameType
                TypeDocItem {..} ->
                    putBindDoc $ let
                        name = showNames diNames
                        in "type " <>
                           mif diStorable "storable " <>
                           case (fmap nameIsInfix $ fullNameRefToUnqualified $ head diNames, diParams) of
                               (Just True, p1:pr) -> toMarkdown p1 <> " " <> name <> trailingParams pr
                               _ -> name <> trailingParams diParams
                SubtypeRelationDocItem {..} ->
                    putBindDoc $ "subtype " <> toMarkdown diSubtype <> " <: " <> toMarkdown diSupertype
                HeadingDocItem {..} -> putIndentMarkdown $ titleMarkdown hlevel diTitle
            if docDescription == ""
                then return ()
                else putIndentMarkdown $ indentMarkdown $ paragraphMarkdown $ rawMarkdown docDescription
            let
                (hlevel', ilevel') =
                    case docItem of
                        HeadingDocItem {} -> (succ hlevel, ilevel)
                        _ -> (hlevel, succ ilevel)
            for_ children $ runDocTree hlevel' ilevel'
        headingTitle :: MarkdownText
        headingTitle =
            case tmodname of
                "pinafore" -> plainText "Built In"
                _ -> plainText $ "import \\\"" <> tmodname <> "\\\""
        headingItem :: DefDoc
        headingItem = MkDefDoc (HeadingDocItem headingTitle) ""
        tree :: Tree DefDoc
        tree = MkTree headingItem $ moduleDoc pmodule
    runDocTree 1 0 $ trimDoc $ deepMergeTree (eqMergeOn docItem) tree
