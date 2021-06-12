module Documentation
    ( printLibraryBindings
    , printInfixOperatorTable
    ) where

import Pinafore.Documentation
import Pinafore.Language
import Shapes
import System.FilePath

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

printLibraryBindings :: [LibraryModule] -> FilePath -> IO ()
printLibraryBindings extralib dirpath =
    for_ (libraryDoc extralib) $ \lib ->
        withBinaryFile (dirpath </> unpack (docTreeName lib) <> ".md") WriteMode $ \h ->
            runDocTree (showDefTitle h) (showDefDesc h) (showDefEntry h) 1 lib

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
