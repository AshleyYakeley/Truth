module Options
    ( Options(..)
    , getOptions
    ) where

import Options.Applicative
import Shapes

data Options
    = ShowVersionOption
    | PredefinedDocOption
    | InfixDocOption
    | DumpTableOption (Maybe FilePath)
    | RunFileOption Bool
                    (Maybe FilePath)
                    (Maybe FilePath)
    | RunInteractiveOption (Maybe FilePath)

optDataFlag :: Parser (Maybe FilePath)
optDataFlag = optional $ strOption $ long "data" <> metavar "PATH"

optParser :: Parser Options
optParser =
    (flag' ShowVersionOption $ long "version" <> short 'v') <|>
    (RunFileOption <$> (switch $ long "no-run" <> short 'n') <*> optDataFlag <*>
     (optional $ strArgument $ metavar "SCRIPT")) <|>
    ((flag' RunInteractiveOption $ long "interactive" <> short 'i') <*> optDataFlag) <|>
    (flag' PredefinedDocOption $ long "doc-predefined") <|>
    (flag' InfixDocOption $ long "doc-infix") <|>
    ((flag' DumpTableOption $ long "dump-table") <*> optDataFlag)

getOptions :: IO Options
getOptions = execParser (info optParser mempty)
