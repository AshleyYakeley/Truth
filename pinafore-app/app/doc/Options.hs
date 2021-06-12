module Options
    ( Options(..)
    , getOptions
    , optParserInfo
    ) where

import Options.Applicative as OA
import Shapes

data Options
    = ShowVersionOption
    | LibraryDocOption FilePath
    | InfixDocOption
    deriving (Eq, Show)

optParser :: Parser Options
optParser =
    (flag' ShowVersionOption $ long "version" <> short 'v') <|>
    (fmap LibraryDocOption $ strOption $ long "doc-library" <> metavar "PATH") <|>
    (flag' InfixDocOption $ long "doc-infix")

optParserInfo :: ParserInfo Options
optParserInfo = info optParser mempty

getOptions :: IO Options
getOptions = execParser optParserInfo
