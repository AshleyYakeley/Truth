module Options
    ( Options(..)
    , RunOptions(..)
    , getOptions
    , optParserInfo
    ) where

import Options.Applicative as OA
import Pinafore.Options
import Shapes

data Options
    = ShowVersionOption
    | ModuleDocOption RunOptions
                      Text
    deriving (Eq, Show)

optIncludes :: Parser [FilePath]
optIncludes = many $ strOption $ long "include" <> short 'I' <> metavar "PATH"

optDataPath :: Parser (Maybe FilePath)
optDataPath = optional $ strOption $ long "data" <> metavar "PATH"

optCache :: Parser Bool
optCache = pure False

optRunOptions :: Parser RunOptions
optRunOptions = MkRunOptions <$> optCache <*> optIncludes <*> optDataPath

optParser :: Parser Options
optParser =
    choice
        [ flag' ShowVersionOption $ long "version" <> short 'v'
        , ModuleDocOption <$> optRunOptions <*> (strArgument $ metavar "MODULENAME")
        ]

optParserInfo :: ParserInfo Options
optParserInfo = info optParser mempty

getOptions :: IO Options
getOptions = execParser optParserInfo
