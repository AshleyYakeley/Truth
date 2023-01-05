module Main
    ( main
    ) where

import Options
import Pinafore.Documentation
import Pinafore.Language
import Pinafore.Libs
import Pinafore.Main
import Pinafore.Version
import Shapes

isSubtypeRel :: Tree DefDoc -> Bool
isSubtypeRel (Node (MkDefDoc SubtypeRelationDocItem {} _) _) = True
isSubtypeRel _ = False

trimDocL :: Tree DefDoc -> [Tree DefDoc]
trimDocL (Node n children) =
    case (docItem n, trimDocChildren children) of
        (HeadingDocItem {}, children')
            | all isSubtypeRel children' -> children'
        (NamespaceDocItem {}, children')
            | all isSubtypeRel children' -> children'
        (_, children') -> [Node n children']

trimDocChildren :: [Tree DefDoc] -> [Tree DefDoc]
trimDocChildren children = children >>= trimDocL

trimDoc :: Tree DefDoc -> Tree DefDoc
trimDoc (Node n children) = Node n $ trimDocChildren children

printModuleDoc :: ModuleOptions -> Text -> IO ()
printModuleDoc modopts tmodname = do
    let fmodule = standardFetchModule modopts
    let ?library = mkLibraryContext nullInvocationInfo fmodule
    let modname = MkModuleName tmodname
    mmod <- fromInterpretResult $ runPinaforeScoped (unpack tmodname) $ lcLoadModule ?library modname
    pmodule <- maybeToM (unpack $ tmodname <> ": not found") mmod
    let
        runDocTree :: Int -> Int -> Namespace -> Tree DefDoc -> IO ()
        runDocTree hlevel ilevel curns (Node MkDefDoc {..} children) = do
            let
                putMarkdown :: Markdown -> IO ()
                putMarkdown m = hPutStr stdout $ unpack $ toText m
                putIndentMarkdown :: Markdown -> IO ()
                putIndentMarkdown m = putMarkdown $ indentMarkdownN ilevel m
                mapNamespaceRef :: NamespaceRef -> Namespace
                mapNamespaceRef fn = namespaceConcatRef RootNamespace fn
                mapFullNameRef :: FullNameRef -> FullName
                mapFullNameRef fn = namespaceConcatFullName RootNamespace fn
                toMarkdown ::
                       forall a. ToNamedText a
                    => a
                    -> MarkdownText
                toMarkdown = plainText . runRelativeNamedText (toList $ namespaceAncestry curns) . toNamedText
                trailingParams ::
                       forall a. ToNamedText a
                    => [a]
                    -> MarkdownText
                trailingParams pp = mconcat $ fmap (\p -> " " <> toMarkdown p) pp
                putBindDoc :: MarkdownText -> IO ()
                putBindDoc m = putIndentMarkdown $ paragraphMarkdown $ codeMarkdown m
            case docItem of
                ValueDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown $ mapFullNameRef diName
                        nameType = name <> " : " <> toMarkdown diType
                        in nameType
                SignatureDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown diSigName
                        nameType = name <> " : " <> toMarkdown diType
                        in nameType
                ValuePatternDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown $ mapFullNameRef diName
                        nameType = name <> " : " <> toMarkdown diType
                        in nameType
                SpecialFormDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown $ mapFullNameRef diName
                        params = trailingParams diParams
                        nameType = name <> params <> ": " <> toMarkdown diType
                        in nameType
                TypeDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown $ mapFullNameRef diName
                        in "type " <>
                           case (fmap nameIsInfix $ fullNameRefToUnqualified diName, diParams) of
                               (Just True, p1:pr) -> toMarkdown p1 <> " " <> name <> trailingParams pr
                               _ -> name <> trailingParams diParams
                SubtypeRelationDocItem {..} ->
                    putBindDoc $ "subtype " <> toMarkdown diSubtype <> " <: " <> toMarkdown diSupertype
                NamespaceDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown $ mapNamespaceRef diNamespace
                        in "namespace " <> name
                HeadingDocItem {..} -> putIndentMarkdown $ titleMarkdown hlevel diTitle
            if docDescription == ""
                then return ()
                else putIndentMarkdown $ indentMarkdown $ paragraphMarkdown $ rawMarkdown docDescription
            let
                (hlevel', ilevel') =
                    case docItem of
                        HeadingDocItem {} -> (succ hlevel, ilevel)
                        _ -> (hlevel, succ ilevel)
                curns' =
                    case docItem of
                        NamespaceDocItem {..} -> namespaceConcatRef RootNamespace diNamespace
                        _ -> curns
            for_ children $ runDocTree hlevel' ilevel' curns'
    runDocTree 1 0 RootNamespace $ trimDoc $ moduleDoc pmodule

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
            for_ mnames $ \n -> putStr $ " `" <> show n <> "`"
            putStr " |"
        putStrLn ""

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
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
