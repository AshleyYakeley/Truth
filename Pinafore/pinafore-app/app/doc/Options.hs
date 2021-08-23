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
    | InfixDocOption
    deriving (Eq, Show)

optIncludes :: Parser [FilePath]
optIncludes = many $ strOption $ long "include" <> short 'I' <> metavar "PATH"

optParser :: Parser Options
optParser =
    (flag' ShowVersionOption $ long "version" <> short 'v') <|>
    (ModuleDocOption <$> optIncludes <*> (strOption $ long "module" <> metavar "MODULENAME")) <|>
    (flag' InfixDocOption $ long "infix")

optParserInfo :: ParserInfo Options
optParserInfo = info optParser mempty

getOptions :: IO Options
getOptions = execParser optParserInfo
