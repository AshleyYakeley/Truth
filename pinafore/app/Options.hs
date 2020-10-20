module Options
    ( Options(..)
    , getOptions
    , optParserInfo
    ) where

import Options.Applicative as OA
import Options.Applicative.Builder.Internal as OA
import Options.Applicative.Types as OA
import Shapes

remainingParser :: Mod CommandFields a -> Parser (String, [String])
remainingParser (Mod _ _ modprops) = let
    remainingParserInfo :: ParserInfo [String]
    remainingParserInfo = (info (many $ strArgument mempty) mempty) {infoPolicy = AllPositionals}
    optMain = CmdReader Nothing [] $ \s -> Just $ fmap ((,) s) remainingParserInfo
    propVisibility = Visible
    propHelp = mempty
    propMetaVar = ""
    propShowDefault = Nothing
    propDescMod = Nothing
    optProps = modprops OptProperties {..}
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

optParser :: Parser Options
optParser =
    (flag' ShowVersionOption $ long "version" <> short 'v') <|>
    ((flag' RunInteractiveOption $ long "interactive" <> short 'i') <*> optDataFlag) <|>
    (flag' PredefinedDocOption $ long "doc-predefined") <|>
    (flag' InfixDocOption $ long "doc-infix") <|>
    ((flag' DumpTableOption $ long "dump-table") <*> optDataFlag) <|>
    (RunFileOption <$> (switch $ long "no-run" <> short 'n') <*> optDataFlag <*>
     fmap Just (remainingParser $ metavar "PATH"))

optParserInfo :: ParserInfo Options
optParserInfo = info optParser mempty

getOptions :: IO Options
getOptions = execParser optParserInfo
