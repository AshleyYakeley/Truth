module Options
    ( Options (..)
    , getOptions
    , optParserInfo
    )
where

import Options.Applicative as OA
import Shapes

data Options
    = ShowVersionOption
    | SyntaxDataDocOption
    | InfixDocOption
    | TypeInfixDocOption
    deriving stock (Eq, Show)

optParser :: Parser Options
optParser =
    choice
        [ flag' ShowVersionOption $ long "version" <> short 'v'
        , flag' SyntaxDataDocOption $ long "syntax-data"
        , flag' InfixDocOption $ long "infix"
        , flag' TypeInfixDocOption $ long "infix-type"
        ]

optParserInfo :: ParserInfo Options
optParserInfo = info optParser mempty

getOptions :: IO Options
getOptions = execParser optParserInfo
