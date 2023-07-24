module Options
    ( Options(..)
    , getOptions
    , optParserInfo
    ) where

import Options.Applicative as OA
import Shapes

data Options
    = ShowVersionOption
    | KeywordsDocOption
    | TypesDocOption
    | ModuleDocOption [FilePath]
                      Text
    | InfixDocOption
    | TypeInfixDocOption
    deriving (Eq, Show)

optIncludes :: Parser [FilePath]
optIncludes = many $ strOption $ long "include" <> short 'I' <> metavar "PATH"

optParser :: Parser Options
optParser =
    choice
        [ flag' ShowVersionOption $ long "version" <> short 'v'
        , flag' KeywordsDocOption $ long "keywords"
        , flag' TypesDocOption $ long "types"
        , ModuleDocOption <$> optIncludes <*> (strOption $ long "module" <> metavar "MODULENAME")
        , flag' InfixDocOption $ long "infix"
        , flag' TypeInfixDocOption $ long "infix-type"
        ]

optParserInfo :: ParserInfo Options
optParserInfo = info optParser mempty

getOptions :: IO Options
getOptions = execParser optParserInfo
