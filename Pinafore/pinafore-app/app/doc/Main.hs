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

showDefEntry :: Handle -> Int -> DefDoc -> IO ()
showDefEntry h _ MkDefDoc {..} = do
    let
        title =
            case docItem of
                ValueDocItem {..} -> let
                    name = boldMarkdown $ codeMarkdown $ toText diName
                    nameType = name <> " " <> codeMarkdown (": " <> diType)
                    in nameType
                ValuePatternDocItem {..} -> let
                    name = boldMarkdown $ codeMarkdown $ toText diName
                    nameType = name <> " " <> codeMarkdown (": " <> diType)
                    in nameType <> " (also pattern)"
                SpecialFormDocItem {..} -> let
                    name = boldMarkdown $ codeMarkdown $ toText diName
                    params = mconcat (fmap (\p -> " " <> codeMarkdown p) diParams)
                    nameType = name <> params <> " " <> codeMarkdown (": " <> diType)
                    in nameType
                TypeDocItem {..} -> let
                    name = boldMarkdown $ codeMarkdown $ toText diName
                    in codeMarkdown "type" <>
                       " " <>
                       case (fmap nameIsInfix $ fullNameToRoot diName, diParams) of
                           (Just True, p1:pr) ->
                               codeMarkdown p1 <> " " <> name <> mconcat (fmap (\p -> " " <> codeMarkdown p) pr)
                           _ -> name <> mconcat (fmap (\p -> " " <> codeMarkdown p) diParams)
                SupertypeDocItem {..} -> let
                    name = boldMarkdown $ codeMarkdown $ toText diName
                    nameType = name <> " " <> codeMarkdown (": " <> diType)
                    in italicMarkdown nameType
                SubtypeRelationDocItem {..} ->
                    codeMarkdown "subtype" <>
                    " " <> codeMarkdown diSubtype <> " " <> codeMarkdown "<:" <> " " <> codeMarkdown diSupertype
    hPutMarkdownLn h $ title <> "  "
    if docDescription == ""
        then return ()
        else hPutMarkdownLn h docDescription
    hPutMarkdownLn h ""

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
    let ?qcontext = nullQContext
    let fmodule = standardFetchModule modopts
    let ?library = mkLibraryContext fmodule
    let modname = MkModuleName tmodname
    mmod <- fromInterpretResult $ runPinaforeScoped (unpack tmodname) $ lcLoadModule ?library modname
    pmodule <- maybeToM (unpack $ tmodname <> ": not found") mmod
    runDocTree (showDefTitle stdout) (showDefDesc stdout) (showDefEntry stdout) 1 $ moduleDoc pmodule

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
