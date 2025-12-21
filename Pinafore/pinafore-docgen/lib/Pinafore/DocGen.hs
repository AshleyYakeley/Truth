module Pinafore.DocGen
    ( generateCommonMarkDoc
    )
where

import Pinafore.Base
import Pinafore.Documentation
import Shapes

isSubtypeRel :: Tree DefDoc -> Bool
isSubtypeRel (MkTree (MkDefDoc SubtypeRelationDocItem{} _) _) = True
isSubtypeRel _ = False

trimDocL :: Tree DefDoc -> Forest DefDoc
trimDocL (MkTree n children) =
    case (docItem n, trimDocChildren children) of
        (HeadingDocItem{}, children'@(MkForest tt))
            | all isSubtypeRel tt -> children'
        (_, children') -> pureForest $ MkTree n children'

trimDocChildren :: Forest DefDoc -> Forest DefDoc
trimDocChildren children = bindForest children trimDocL

trimDoc :: Tree DefDoc -> Tree DefDoc
trimDoc (MkTree n children) = MkTree n $ trimDocChildren children

fors_ :: Monad m => s -> [a] -> (s -> a -> m s) -> m ()
fors_ olds aa f =
    case aa of
        [] -> return ()
        a : ar -> do
            news <- f olds a
            fors_ news ar f

generateCommonMarkDoc :: Handle -> ModuleOptions -> ModuleName -> IO ()
generateCommonMarkDoc outh modopts modname = do
    let ?library = standardLibraryContext modopts
    docs <- getModuleDocs modname
    let
        runDocTree :: Int -> Int -> Bool -> Tree DefDoc -> IO Bool
        runDocTree hlevel ilevel oldHeading (MkTree MkDefDoc{..} (MkForest children)) = do
            let
                putMarkdown :: Markdown -> IO ()
                putMarkdown m = hPutStr outh $ unpack $ toText m
                putIndentMarkdown :: Markdown -> IO ()
                putIndentMarkdown m = putMarkdown $ indentMarkdownN ilevel m
                mapFullNameRef :: FullNameRef -> FullName
                mapFullNameRef fn = namespaceConcatFullName RootNamespace fn
                toMarkdown :: NamedText -> MarkdownText
                toMarkdown = plainText . runRelativeNamedText [RootNamespace]
                showMarkdown ::
                    forall a.
                    ShowNamedText a =>
                    a ->
                    MarkdownText
                showMarkdown = toMarkdown . showNamedText
                trailing :: [NamedText] -> MarkdownText
                trailing pp = concatmap (\p -> " " <> toMarkdown p) pp
                putBindDoc :: MarkdownText -> IO ()
                putBindDoc m = putIndentMarkdown $ paragraphMarkdown $ codeMarkdown m
                showNames :: NonEmpty FullNameRef -> MarkdownText
                showNames names = intercalate ", " $ toList $ fmap (boldMarkdown . showMarkdown . mapFullNameRef) names
            let
                newHeading =
                    case docItem of
                        HeadingDocItem{} -> True
                        _ -> False
            if oldHeading && not newHeading
                then putIndentMarkdown $ titleMarkdown hlevel "Other"
                else return ()
            case docItem of
                ValueDocItem{..} ->
                    putBindDoc $ let
                        name = showNames diNames
                        nameType = name <> " : " <> toMarkdown diType
                        in nameType
                ValueSignatureDocItem{..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ showMarkdown diSigName
                        nameType = name <> " : " <> toMarkdown diType
                        in nameType
                            <> if diHasDefault
                                then " [optional]"
                                else ""
                SupertypeConstructorSignatureDocItem{..} -> putBindDoc $ boldMarkdown $ showMarkdown diName
                ValuePatternDocItem{..} ->
                    putBindDoc $ let
                        name = showNames diNames
                        nameType = name <> " : " <> toMarkdown diType
                        in nameType
                SpecialFormDocItem{..} ->
                    putBindDoc $ let
                        name = showNames diNames
                        params = trailing diAnnotations
                        nameType = name <> params <> ": " <> toMarkdown diType
                        in nameType
                TypeDocItem{..} ->
                    putIndentMarkdown
                        $ paragraphMarkdown
                        $ let
                            name = showNames diNames
                            in ( codeMarkdown
                                    $ "type "
                                    <> mif diStorable "storable "
                                    <> case (fmap nameIsInfix $ fullNameRefToUnqualified $ head diNames, diParams) of
                                        (Just True, p1 : pr) ->
                                            toMarkdown (showNamedText p1) <> " " <> name <> trailing (fmap showNamedText pr)
                                        _ -> name <> trailing (fmap showNamedText diParams)
                                    <> case diEquivalentDefn of
                                        Nothing -> ""
                                        Just syn -> " = " <> toMarkdown syn
                               )
                                <> case diGDS of
                                    Just (_, gds) -> " (from " <> codeMarkdown (toMarkdown gds) <> ")"
                                    Nothing -> mempty
                SubtypeRelationDocItem{..} ->
                    putBindDoc $ "subtype " <> toMarkdown diSubtype <> " <: " <> toMarkdown diSupertype
                HeadingDocItem{..} -> putIndentMarkdown $ titleMarkdown hlevel diTitle
            if docDescription == ""
                then return ()
                else putIndentMarkdown $ indentMarkdown $ paragraphMarkdown $ rawMarkdown docDescription
            let
                (hlevel', ilevel') =
                    if newHeading
                        then (succ hlevel, ilevel)
                        else (hlevel, succ ilevel)
            fors_ False children $ runDocTree hlevel' ilevel'
            return newHeading
        headingTitle :: MarkdownText
        headingTitle =
            case modname of
                "pinafore" -> plainText "Built In"
                _ -> plainText $ "import \\\"" <> showText modname <> "\\\""
        headingItem :: DefDoc
        headingItem = MkDefDoc (HeadingDocItem headingTitle) ""
        tree :: Tree DefDoc
        tree = MkTree headingItem docs
    _ <- runDocTree 1 0 False $ trimDoc $ deepMergeTree (eqMergeOn docItem) tree
    return ()
