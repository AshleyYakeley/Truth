module Documentation
    ( printLibraryBindings
    , printInfixOperatorTable
    ) where

import Pinafore.Documentation
import Pinafore.Language
import Shapes
import System.FilePath

escapeMarkdown :: String -> String
escapeMarkdown s = let
    badchars :: String
    badchars = "+*>\\"
    escapeChar :: Char -> String
    escapeChar c =
        if elem c badchars
            then ['\\', c]
            else [c]
    in mconcat $ fmap escapeChar s

showDefEntry :: Handle -> Int -> DefDoc -> IO ()
showDefEntry h _ MkDefDoc {..} = do
    let
        name = "**`" <> unpack docName <> "`**"
        nameType = name <> " `: " <> unpack docValueType <> "`"
        title =
            case docType of
                ValueDocType -> nameType
                ValuePatternDocType -> nameType <> " (also pattern)"
                TypeDocType -> "`type` " <> name
                SupertypeDocType -> "_" <> nameType <> "_"
                SubtypeRelationDocType -> "`subtype` " <> name
    hPutStrLn h $ title <> "  "
    if docDescription == ""
        then return ()
        else hPutStrLn h $ escapeMarkdown $ unpack docDescription
    hPutStrLn h ""

showDefTitle :: Handle -> Int -> Text -> IO ()
showDefTitle _ 1 "" = return ()
showDefTitle h level title = hPutStrLn h $ replicate level '#' <> " " <> unpack title

showDefDesc :: Handle -> Int -> Text -> IO ()
showDefDesc _ _ "" = return ()
showDefDesc h _ desc = do
    hPutStrLn h $ unpack desc
    hPutStrLn h ""

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
