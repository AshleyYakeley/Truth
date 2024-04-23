module Options
    ( Options(..)
    , getOptions
    , optParserInfo
    ) where

import Options.Applicative as OA
import Shapes

data Options
    = ShowVersionOption
    | ModuleDocOption [FilePath]
                      Text
    deriving (Eq, Show)

optIncludes :: Parser [FilePath]
optIncludes = many $ strOption $ long "include" <> short 'I' <> metavar "PATH"

optParser :: Parser Options
optParser =
    choice
        [ flag' ShowVersionOption $ long "version" <> short 'v'
        , ModuleDocOption <$> optIncludes <*> (strArgument $ metavar "MODULENAME")
        ]

optParserInfo :: ParserInfo Options
optParserInfo = info optParser mempty

getOptions :: IO Options
getOptions = execParser optParserInfo
