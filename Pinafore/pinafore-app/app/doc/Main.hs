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

hPutMarkdownLn :: Handle -> Markdown -> IO ()
hPutMarkdownLn h m = hPutStrLn h $ unpack $ getRawMarkdown m

showDefDoc :: Handle -> Int -> DefDoc -> IO ()
showDefDoc h i MkDefDoc {..} = do
    let
        title =
            case docItem of
                ValueDocItem {..} -> let
                    name = boldMarkdown $ codeMarkdown $ toText diName
                    nameType = name <> " " <> codeMarkdown (": " <> toText diType)
                    in nameType
                ValuePatternDocItem {..} -> let
                    name = boldMarkdown $ codeMarkdown $ toText diName
                    nameType = name <> " " <> codeMarkdown (": " <> toText diType)
                    in nameType
                SpecialFormDocItem {..} -> let
                    name = boldMarkdown $ codeMarkdown $ toText diName
                    params = mconcat (fmap (\p -> " " <> codeMarkdown (toText p)) diParams)
                    nameType = name <> params <> " " <> codeMarkdown (": " <> toText diType)
                    in nameType
                TypeDocItem {..} -> let
                    name = boldMarkdown $ codeMarkdown $ toText diName
                    in codeMarkdown "type" <>
                       " " <>
                       case (fmap nameIsInfix $ fullNameRefToUnqualified diName, diParams) of
                           (Just True, p1:pr) ->
                               codeMarkdown (toText p1) <>
                               " " <> name <> mconcat (fmap (\p -> " " <> codeMarkdown (toText p)) pr)
                           _ -> name <> mconcat (fmap (\p -> " " <> codeMarkdown (toText p)) diParams)
                SupertypeDocItem {..} -> let
                    name = boldMarkdown $ codeMarkdown $ toText diName
                    nameType = name <> " " <> codeMarkdown (": " <> toText diType)
                    in italicMarkdown nameType
                SubtypeRelationDocItem {..} ->
                    codeMarkdown "subtype" <>
                    " " <>
                    codeMarkdown (toText diSubtype) <>
                    " " <> codeMarkdown "<:" <> " " <> codeMarkdown (toText diSupertype)
    hPutMarkdownLn h $ indentMarkdownN i title <> "  "
    if docDescription == ""
        then return ()
        else hPutMarkdownLn h $ indentMarkdownN i docDescription
    hPutMarkdownLn h ""

showDefEntry :: Handle -> Int -> Int -> Tree DefDoc -> IO ()
showDefEntry h i _ (Node d tt) = do
    showDefDoc h i d
    for_ tt $ showDefEntry h (succ i) 0

showDefTitle :: Handle -> Int -> Text -> IO ()
showDefTitle _ 1 "" = return ()
showDefTitle h level title = hPutMarkdownLn h $ titleMarkdown level $ plainMarkdown title

showDefDesc :: Handle -> Int -> Markdown -> IO ()
showDefDesc _ _ "" = return ()
showDefDesc h _ desc = do
    hPutMarkdownLn h desc
    hPutMarkdownLn h ""

printModuleDoc :: ModuleOptions -> Text -> IO ()
printModuleDoc modopts tmodname = do
    let fmodule = standardFetchModule modopts
    let ?library = mkLibraryContext nullInvocationInfo fmodule
    let modname = MkModuleName tmodname
    mmod <- fromInterpretResult $ runPinaforeScoped (unpack tmodname) $ lcLoadModule ?library modname
    pmodule <- maybeToM (unpack $ tmodname <> ": not found") mmod
    runDocTree (showDefTitle stdout) (showDefDesc stdout) (showDefEntry stdout 0) 1 $ moduleDoc pmodule

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
