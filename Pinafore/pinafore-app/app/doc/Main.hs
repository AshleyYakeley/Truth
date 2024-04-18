module Main
    ( main
    ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.Aeson.Text as JSON
import Options
import Pinafore.Documentation
import Pinafore.Language
import Pinafore.Libs
import Pinafore.Main
import Pinafore.Version
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

printModuleDoc :: ModuleOptions -> Text -> IO ()
printModuleDoc modopts tmodname = do
    let fmodule = standardFetchModule modopts
    let ?library = mkLibraryContext nullInvocationInfo fmodule mempty
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

printInfixOperatorTable :: [(Name, Fixity)] -> IO ()
printInfixOperatorTable fixities = do
    let
        maxLevel :: Int
        maxLevel = maximum $ fmap (fixityPrec . snd) fixities
    putStrLn "| [n] | (A x B) x C | A x (B x C) | A x B only |"
    putStrLn "| --- | --- | --- | --- |"
    for_ [maxLevel,pred maxLevel .. 0] $ \level -> do
        putStr $ "| " <> show level <> " |"
        for_ [AssocLeft, AssocRight, AssocNone] $ \assc -> do
            let
                fixity = MkFixity assc level
                mnames =
                    mapMaybe
                        (\(n, f) ->
                             if f == fixity
                                 then Just n
                                 else Nothing)
                        fixities
            putStr $ unpack $ toText $ codeMarkdown $ plainText $ intercalate "  " $ fmap showText mnames
            putStr " |"
        putStrLn ""

syntaxData :: JSON.Value
syntaxData = let
    groups :: [Text]
    groups = sort $ nub $ fmap snd allKeywords
    items :: Text -> [Text]
    items gp =
        sort $
        mapMaybe
            (\(n, g) ->
                 if gp == g
                     then Just n
                     else Nothing)
            allKeywords
    keywords :: JSON.Value
    keywords =
        JSON.Object $
        JSON.fromList $ fmap (\g -> (JSON.fromText g, JSON.Array $ fromList $ fmap JSON.String $ items g)) groups
    types :: JSON.Value
    types = JSON.Array $ fromList $ fmap (JSON.String . showText) $ ["Any", "None"] <> allTypeNames
    in JSON.Object $ JSON.fromList $ [("keywords", keywords), ("types", types)]

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        SyntaxDataDocOption -> putStrLn $ unpack $ JSON.encodeToLazyText $ syntaxData
        ModuleDocOption moModuleDirs modname -> let
            moExtraLibrary = extraLibrary
            in printModuleDoc MkModuleOptions {..} modname
        InfixDocOption ->
            printInfixOperatorTable $
            fmap (\n -> (n, operatorFixity n)) $
            allOperatorNames $ \case
                ValueDocItem {} -> True
                _ -> False
        TypeInfixDocOption ->
            printInfixOperatorTable $
            fmap (\n -> (n, typeOperatorFixity n)) $
            allOperatorNames $ \case
                TypeDocItem {} -> True
                _ -> False
