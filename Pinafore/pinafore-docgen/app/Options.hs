module Options
    ( Options(..)
    , RunOptions(..)
    , getOptions
    , optParserInfo
    ) where

import Options.Applicative as OA
import Pinafore.Language.API
import Pinafore.Options
import Shapes

data Options
    = ShowVersionOption
    | ModuleDocOption RunOptions
                      ModuleSpec

optIncludes :: Parser [FilePath]
optIncludes = many $ strOption $ long "include" <> short 'I' <> metavar "PATH"

optDataPath :: Parser (Maybe FilePath)
optDataPath = optional $ strOption $ long "data" <> metavar "PATH"

optCache :: Parser Bool
optCache = pure False

optRunOptions :: Parser RunOptions
optRunOptions = MkRunOptions <$> optCache <*> optIncludes <*> optDataPath

toModuleSpec :: Maybe Text -> Text -> ModuleSpec
toModuleSpec Nothing n = PlainModuleSpec $ MkModuleName n
toModuleSpec (Just t) n = SpecialModuleSpec (MkName t) n

optModuleSpec :: Parser ModuleSpec
optModuleSpec =
    toModuleSpec <$> (optional $ strOption $ long "type" <> metavar "TYPE") <*> (strArgument $ metavar "MODULENAME")

optParser :: Parser Options
optParser =
    choice [flag' ShowVersionOption $ long "version" <> short 'v', ModuleDocOption <$> optRunOptions <*> optModuleSpec]

optParserInfo :: ParserInfo Options
optParserInfo = info optParser mempty

getOptions :: IO Options
getOptions = execParser optParserInfo
