module Options
    ( Options(..)
    , RunOptions(..)
    , getOptions
    , optParserInfo
    ) where

import Options.Applicative as OA
import Options.Applicative.Builder.Internal as OA
import Options.Applicative.Types as OA
import Pinafore.Options
import Shapes

remainingParser :: OA.Mod CommandFields a -> Parser (String, [String])
remainingParser (Mod _ _ modprops) = let
    remainingParserInfo :: ParserInfo [String]
    remainingParserInfo = (info (many $ strArgument mempty) mempty) {infoPolicy = AllPositionals}
    matchCmd ('-':_) = Nothing
    matchCmd s = Just $ fmap ((,) s) remainingParserInfo
    optMain = CmdReader Nothing [] matchCmd
    propVisibility = Visible
    propHelp = mempty
    propMetaVar = ""
    propShowDefault = Nothing
    propDescMod = Nothing
    propShowGlobal = False
    optProps = modprops OptProperties {..}
    in OptP OA.Option {..}

data Options
    = ShowVersionOption
    | DumpTableOption (Maybe FilePath)
    | RunFileOption RunOptions
                    Bool
                    (FilePath, [String], [(Text, Text)])
    | RunInteractiveOption RunOptions
    deriving (Eq, Show)

optIncludes :: Parser [FilePath]
optIncludes = many $ strOption $ long "include" <> short 'I' <> metavar "PATH"

optDataPath :: Parser (Maybe FilePath)
optDataPath = optional $ strOption $ long "data" <> metavar "PATH"

optScript :: Parser (String, [String])
optScript = remainingParser $ metavar "PATH"

optNoRun :: Parser Bool
optNoRun = switch $ long "no-run" <> short 'n'

optRunOptions :: Parser RunOptions
optRunOptions = MkRunOptions <$> optIncludes <*> optDataPath <*> switch (long "sloppy")

assignReader :: ReadM (Text, Text)
assignReader =
    maybeReader $ \s -> let
        (t, d) = span (\c -> c /= '=') s
        in case d of
               '=':v -> Just (pack t, pack v)
               _ -> Nothing

optImply :: Parser (Text, Text)
optImply = option assignReader $ long "imply" <> metavar "name=value" <> help "imply `?name = value`"

optParser :: Parser Options
optParser =
    (flag' ShowVersionOption $ long "version" <> short 'v') <|>
    (((flag' RunInteractiveOption $ long "interactive" <> short 'i') <|>
      ((\nr implies (fp, args) ropts -> RunFileOption ropts nr (fp, args, implies)) <$> optNoRun <*> many optImply <*>
       optScript)) <*>
     optRunOptions) <|>
    ((flag' DumpTableOption $ long "dump-table") <*> optDataPath)

optParserInfo :: ParserInfo Options
optParserInfo = info optParser mempty

getOptions :: IO Options
getOptions = execParser optParserInfo
