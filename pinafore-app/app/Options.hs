module Options
    ( Options(..)
    , RunOptions(..)
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
    matchCmd ('-':_) = Nothing
    matchCmd s = Just $ fmap ((,) s) remainingParserInfo
    optMain = CmdReader Nothing [] matchCmd
    propVisibility = Visible
    propHelp = mempty
    propMetaVar = ""
    propShowDefault = Nothing
    propDescMod = Nothing
    optProps = modprops OptProperties {..}
    in OptP OA.Option {..}

data RunOptions = MkRunOptions
    { roCache :: Bool
    , roIncludeDirs :: [FilePath]
    , roDataDir :: Maybe FilePath
    } deriving (Eq, Show)

data Options
    = ShowVersionOption
    | LibraryDocOption FilePath
    | InfixDocOption
    | DumpTableOption (Maybe FilePath)
    | RunFileOption RunOptions
                    Bool
                    (FilePath, [String])
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

optCache :: Parser Bool
optCache = fmap not $ switch $ long "no-cache"

optRunOptions :: Parser RunOptions
optRunOptions = MkRunOptions <$> optCache <*> optIncludes <*> optDataPath

optParser :: Parser Options
optParser =
    (flag' ShowVersionOption $ long "version" <> short 'v') <|>
    (((flag' RunInteractiveOption $ long "interactive" <> short 'i') <|>
      ((\nr script ropts -> RunFileOption ropts nr script) <$> optNoRun <*> optScript)) <*>
     optRunOptions) <|>
    (fmap LibraryDocOption $ strOption $ long "doc-library" <> hidden <> metavar "PATH") <|>
    (flag' InfixDocOption $ long "doc-infix" <> hidden) <|>
    ((flag' DumpTableOption $ long "dump-table") <*> optDataPath)

optParserInfo :: ParserInfo Options
optParserInfo = info optParser mempty

getOptions :: IO Options
getOptions = execParser optParserInfo
