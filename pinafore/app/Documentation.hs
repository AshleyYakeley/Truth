module Documentation
    ( printPredefinedBindings
    , printInfixOperatorTable
    ) where

import Pinafore.Language.Documentation
import Shapes

escapeMarkdown :: String -> String
escapeMarkdown s = let
    badchars :: String
    badchars = "+-*>\\"
    escapeChar :: Char -> String
    escapeChar c =
        if elem c badchars
            then ['\\', c]
            else [c]
    in mconcat $ fmap escapeChar s

showDefEntry :: Int -> DefDoc -> IO ()
showDefEntry _ MkDefDoc {..} = do
    let
        nameType = "**`" ++ show docName ++ "`** :: `" ++ unpack docValueType ++ "`"
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
showDefTitle level title = putStrLn $ replicate level '#' ++ " " ++ unpack title

showDefDesc :: Int -> Text -> IO ()
showDefDesc _ "" = return ()
showDefDesc _ desc = do
    putStrLn $ unpack desc
    putStrLn ""

printPredefinedBindings :: IO ()
printPredefinedBindings = runDocTree showDefTitle showDefDesc showDefEntry 1 predefinedDoc

printInfixOperatorTable :: IO ()
printInfixOperatorTable = do
    let
        getDocName MkDefDoc {..}
            | not docIsSupertype
            , nameIsInfix docName = Just docName
        getDocName _ = Nothing
        names = catMaybes $ fmap getDocName $ toList predefinedDoc
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
