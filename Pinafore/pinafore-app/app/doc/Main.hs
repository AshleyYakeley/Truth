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
        name = boldMarkdown $ codeMarkdown docName
        nameType = name <> " " <> codeMarkdown (": " <> docValueType)
        title =
            case docType of
                ValueDocType -> nameType
                ValuePatternDocType -> nameType <> " (also pattern)"
                TypeDocType -> codeMarkdown "type" <> " " <> name
                SupertypeDocType -> italicMarkdown nameType
                SubtypeRelationDocType -> codeMarkdown "subtype" <> " " <> name
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
printModuleDoc modopts tmodname = let
    ?pinafore = nullPinaforeContext
    in do
           let fmodule = standardFetchModule modopts
           lc <- mkLibraryContext fmodule
           let
               ?library = lc
               in do
                      modname <- maybeToM (unpack $ tmodname <> ": bad module name") $ toModuleName tmodname
                      mmod <-
                          throwInterpretResult $
                          runPinaforeScoped (initialPos $ unpack tmodname) $ lcLoadModule lc modname
                      pmodule <- maybeToM (unpack $ tmodname <> ": not found") mmod
                      runDocTree (showDefTitle stdout) (showDefDesc stdout) (showDefEntry stdout) 1 $ moduleDoc pmodule

printInfixOperatorTable :: IO ()
printInfixOperatorTable = do
    putStrLn "| [n] | (A x B) x C | A x (B x C) | A x B only |"
    putStrLn "| --- | --- | --- | --- |"
    for_ [10,9 .. 0] $ \level -> do
        putStr $ show level
        for_ [AssocLeft, AssocRight, AssocNone] $ \assc -> do
            putStr " |"
            let
                fixity = MkFixity assc level
                mnames = filter (\n -> operatorFixity n == fixity) allOperatorNames
            for_ mnames $ \n -> putStr $ " `" <> show n <> "`"
        putStrLn ""

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        ModuleDocOption moModuleDirs modname -> let
            moExtraLibrary = extraLibrary
            in printModuleDoc MkModuleOptions {..} modname
        InfixDocOption -> printInfixOperatorTable
