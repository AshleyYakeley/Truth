module Main
    ( main
    )
where

import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Data.Aeson.Text qualified as JSON
import Pinafore.Base
import Pinafore.Documentation
import Shapes

import Options
import Pinafore.Version

printInfixOperatorTable :: [(Name, Fixity)] -> IO ()
printInfixOperatorTable fixities = do
    let
        maxLevel :: Word
        maxLevel = maximum $ fmap (fixityPrec . snd) fixities
    putStrLn "| [n] | (A x B) x C | A x (B x C) | A x B only |"
    putStrLn "| --- | --- | --- | --- |"
    for_ [maxLevel, pred maxLevel .. 0] $ \level -> do
        putStr $ "| " <> show level <> " |"
        for_ [AssocLeft, AssocRight, AssocNone] $ \assc -> do
            let
                fixity = MkFixity assc level
                mnames =
                    mapMaybe
                        ( \(n, f) ->
                            if f == fixity
                                then Just n
                                else Nothing
                        )
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
        sort
            $ mapMaybe
                ( \(n, g) ->
                    if gp == g
                        then Just n
                        else Nothing
                )
                allKeywords
    keywords :: JSON.Value
    keywords =
        JSON.Object
            $ JSON.fromList
            $ fmap (\g -> (JSON.fromText g, JSON.Array $ fromList $ fmap JSON.String $ items g)) groups
    types :: JSON.Value
    types = JSON.Array $ fromList $ fmap (JSON.String . showText) $ ["Any", "None"] <> allTypeNames
    in JSON.Object $ JSON.fromList $ [("keywords", keywords), ("types", types)]

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        SyntaxDataDocOption -> putStrLn $ unpack $ JSON.encodeToLazyText $ syntaxData
        InfixDocOption ->
            printInfixOperatorTable
                $ fmap (\n -> (n, operatorFixity n))
                $ allOperatorNames
                $ \case
                    ValueDocItem{} -> True
                    _ -> False
        TypeInfixDocOption ->
            printInfixOperatorTable
                $ fmap (\n -> (n, typeOperatorFixity n))
                $ allOperatorNames
                $ \case
                    TypeDocItem{} -> True
                    _ -> False
