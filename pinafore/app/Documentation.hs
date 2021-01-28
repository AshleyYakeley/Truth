module Documentation
    ( printLibraryBindings
    , printInfixOperatorTable
    ) where

import Pinafore.Documentation
import Shapes

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

showDefEntry :: Int -> DefDoc -> IO ()
showDefEntry _ MkDefDoc {..} = do
    let
        nameType = "**`" ++ unpack docName ++ "`** `: " ++ unpack docValueType ++ "`"
        title =
            (if docIsSupertype
                 then "_" <> nameType <> "_"
                 else nameType) <>
            (if docIsPattern
                 then " (also pattern)"
                 else "")
    putStrLn $ title <> "  "
    if docDescription == ""
        then return ()
        else putStrLn $ escapeMarkdown $ unpack docDescription
    putStrLn ""

showDefTitle :: Int -> Text -> IO ()
showDefTitle 1 "" = return ()
showDefTitle level title = putStrLn $ replicate level '#' ++ " " ++ unpack title

showDefDesc :: Int -> Text -> IO ()
showDefDesc _ "" = return ()
showDefDesc _ desc = do
    putStrLn $ unpack desc
    putStrLn ""

printLibraryBindings :: IO ()
printLibraryBindings = runDocTree showDefTitle showDefDesc showDefEntry 1 libraryDoc

printInfixOperatorTable :: IO ()
printInfixOperatorTable = do
    let
        getDocName MkDefDoc {..}
            | not docIsSupertype
            , nameIsInfix (MkName docName) = Just $ MkName docName
        getDocName _ = Nothing
        names = catMaybes $ fmap getDocName $ toList libraryDoc
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
