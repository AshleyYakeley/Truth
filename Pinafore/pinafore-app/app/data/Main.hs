module Main
    ( main
    ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.Aeson.Text as JSON
import Options
import Pinafore.Documentation
import Pinafore.Version
import Shapes

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
            putStr $ unpack $ toText $ codeMarkdown $ plainText $ intercalate "  " $ fmap showText mnames
            putStr " |"
        putStrLn ""

syntaxData :: JSON.Value
syntaxData = let
    groups :: [Text]
    groups = sort $ nub $ fmap snd allKeywords
    items :: Text -> [Text]
    items gp =
        sort $
        mapMaybe
            (\(n, g) ->
                 if gp == g
                     then Just n
                     else Nothing)
            allKeywords
    keywords :: JSON.Value
    keywords =
        JSON.Object $
        JSON.fromList $ fmap (\g -> (JSON.fromText g, JSON.Array $ fromList $ fmap JSON.String $ items g)) groups
    types :: JSON.Value
    types = JSON.Array $ fromList $ fmap (JSON.String . showText) $ ["Any", "None"] <> allTypeNames
    in JSON.Object $ JSON.fromList $ [("keywords", keywords), ("types", types)]

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        SyntaxDataDocOption -> putStrLn $ unpack $ JSON.encodeToLazyText $ syntaxData
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
