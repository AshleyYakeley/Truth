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

printModuleDoc :: ModuleOptions -> Text -> IO ()
printModuleDoc modopts tmodname = do
    let fmodule = standardFetchModule modopts
    let ?library = mkLibraryContext nullInvocationInfo fmodule
    let modname = MkModuleName tmodname
    mmod <- fromInterpretResult $ runPinaforeScoped (unpack tmodname) $ lcLoadModule ?library modname
    pmodule <- maybeToM (unpack $ tmodname <> ": not found") mmod
    let
        runDocTree :: Int -> Int -> Tree DefDoc -> IO ()
        runDocTree hlevel ilevel (Node MkDefDoc {..} children) = do
            let
                putMarkdownLn :: Markdown -> IO ()
                putMarkdownLn m = hPutStrLn stdout $ unpack $ getRawMarkdown m
                putIndentMarkdownLn :: Markdown -> IO ()
                putIndentMarkdownLn m = putMarkdownLn $ indentMarkdownN ilevel m
                toMarkdown ::
                       forall a. ToText a
                    => a
                    -> Markdown
                toMarkdown = plainMarkdown . toText
                trailingParams ::
                       forall a. ToText a
                    => [a]
                    -> Markdown
                trailingParams pp = mconcat $ fmap (\p -> " " <> toMarkdown p) pp
                putBindDoc :: Markdown -> IO ()
                putBindDoc m = putIndentMarkdownLn $ codeMarkdown m <> "  "
            case docItem of
                ValueDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown diName
                        nameType = name <> ": " <> toMarkdown diType
                        in nameType
                ValuePatternDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown diName
                        nameType = name <> ": " <> toMarkdown diType
                        in nameType
                SpecialFormDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown diName
                        params = trailingParams diParams
                        nameType = name <> params <> ": " <> toMarkdown diType
                        in nameType
                TypeDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown diName
                        in "type " <>
                           case (fmap nameIsInfix $ fullNameRefToUnqualified diName, diParams) of
                               (Just True, p1:pr) -> toMarkdown p1 <> " " <> name <> trailingParams pr
                               _ -> name <> trailingParams diParams
                SupertypeDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown diName
                        nameType = name <> ": " <> toMarkdown diType
                        in italicMarkdown nameType
                SubtypeRelationDocItem {..} ->
                    putBindDoc $ "subtype " <> toMarkdown diSubtype <> " <: " <> toMarkdown diSupertype
                NamespaceDocItem {..} ->
                    putBindDoc $ let
                        name = boldMarkdown $ toMarkdown diNamespace
                        in "namespace " <> name
                HeadingDocItem {..} -> putMarkdownLn $ titleMarkdown hlevel diTitle
            if docDescription == ""
                then return ()
                else putIndentMarkdownLn docDescription
            putIndentMarkdownLn ""
            let
                (hlevel', ilevel') =
                    case docItem of
                        HeadingDocItem {} -> (succ hlevel, ilevel)
                        _ -> (hlevel, succ ilevel)
            for_ children $ runDocTree hlevel' ilevel'
    runDocTree 1 0 $ moduleDoc pmodule

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
