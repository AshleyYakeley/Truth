module Options
    ( Options(..)
    , getOptions
    , optParserInfo
    ) where

import Options.Applicative as OA
import Options.Applicative.Types as OA
import Shapes

remainingParser :: Parser [String]
remainingParser = let
    remainingParserInfo :: ParserInfo [String]
    remainingParserInfo = (info (many $ strArgument mempty) mempty) {infoPolicy = AllPositionals}
    optMain = CmdReader Nothing [] $ \s -> Just $ fmap ((:) s) remainingParserInfo
    propVisibility = Internal
    propHelp = mempty
    propMetaVar = ""
    propShowDefault = Nothing
    propDescMod = Nothing
    optProps = OptProperties {..}
    in OptP OA.Option {..}

data Options
    = ShowVersionOption
    | PredefinedDocOption
    | InfixDocOption
    | DumpTableOption (Maybe FilePath)
    | RunFileOption Bool
                    (Maybe FilePath)
                    (Maybe (FilePath, [String]))
    | RunInteractiveOption (Maybe FilePath)
    deriving (Eq, Show)

optDataFlag :: Parser (Maybe FilePath)
optDataFlag = optional $ strOption $ long "data" <> metavar "PATH"

toScript :: [String] -> Maybe (FilePath, [String])
toScript [] = Nothing
toScript (fpath:args) = Just (fpath, args)

optParser :: Parser Options
optParser =
    (flag' ShowVersionOption $ long "version" <> short 'v') <|>
    ((flag' RunInteractiveOption $ long "interactive" <> short 'i') <*> optDataFlag) <|>
    (flag' PredefinedDocOption $ long "doc-predefined") <|>
    (flag' InfixDocOption $ long "doc-infix") <|>
    ((flag' DumpTableOption $ long "dump-table") <*> optDataFlag) <|>
    (RunFileOption <$> (switch $ long "no-run" <> short 'n') <*> optDataFlag <*> fmap toScript remainingParser)

optParserInfo :: ParserInfo Options
optParserInfo = info optParser mempty

getOptions :: IO Options
getOptions = execParser optParserInfo
