module Documentation
    ( printLibraryBindings
    , printInfixOperatorTable
    ) where

import Pinafore.Documentation
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
        nameType = "**`" ++ unpack docName ++ "`** `: " ++ unpack docValueType ++ "`"
        title =
            (if docIsSupertype
                 then "_" <> nameType <> "_"
                 else nameType) <>
            (if docIsPattern
                 then " (also pattern)"
                 else "")
    hPutStrLn h $ title <> "  "
    if docDescription == ""
        then return ()
        else hPutStrLn h $ escapeMarkdown $ unpack docDescription
    hPutStrLn h ""

showDefTitle :: Handle -> Int -> Text -> IO ()
showDefTitle _ 1 "" = return ()
showDefTitle h level title = hPutStrLn h $ replicate level '#' ++ " " ++ unpack title

showDefDesc :: Handle -> Int -> Text -> IO ()
showDefDesc _ _ "" = return ()
showDefDesc h _ desc = do
    hPutStrLn h $ unpack desc
    hPutStrLn h ""

printLibraryBindings :: FilePath -> IO ()
printLibraryBindings dirpath =
    for_ libraryDoc $ \lib ->
        withBinaryFile (dirpath </> unpack (docTreeName lib) <> ".md") WriteMode $ \h ->
            runDocTree (showDefTitle h) (showDefDesc h) (showDefEntry h) 1 lib

printInfixOperatorTable :: IO ()
printInfixOperatorTable = do
    let
        getDocName MkDefDoc {..}
            | not docIsSupertype
            , nameIsInfix (MkName docName) = Just $ MkName docName
        getDocName _ = Nothing
        names = catMaybes $ fmap getDocName $ mconcat $ fmap toList libraryDoc
    putStrLn "| [n] | (A x B) x C | A x (B x C) | A x B only |"
    putStrLn "| --- | --- | --- | --- |"
    for_ [10,9 .. 0] $ \level -> do
        putStr $ show level
        for_ [AssocLeft, AssocRight, AssocNone] $ \assc -> do
            putStr " |"
            let
                fixity = MkFixity assc level
                mnames = filter (\n -> operatorFixity n == fixity) names
            for_ mnames $ \n -> putStr $ " `" <> show n <> "`"
        putStrLn ""
