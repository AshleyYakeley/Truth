module Options
    ( Options(..)
    , getOptions
    ) where

import Options.Applicative
import Shapes
import Truth.Core

data Options
    = ShowVersionOption
    | PredefinedDocOption
    | InfixDocOption
    | DumpTableOption (Maybe FilePath)
    | RunFileOption UpdateTiming
                    Bool
                    (Maybe FilePath)
                    [FilePath]
    | RunInteractiveOption UpdateTiming
                           (Maybe FilePath)

optDataFlag :: Parser (Maybe FilePath)
optDataFlag = optional $ strOption $ long "data" <> metavar "PATH"

optSyncFlag :: Parser UpdateTiming
optSyncFlag =
    fmap
        (\f ->
             if f
                 then SynchronousUpdateTiming
                 else AsynchronousUpdateTiming) $
    switch $ long "sync"

optParser :: Parser Options
optParser =
    (flag' ShowVersionOption $ long "version" <> short 'v') <|>
    (RunFileOption <$> optSyncFlag <*> (switch $ long "no-run" <> short 'n') <*> optDataFlag <*>
     (many $ strArgument $ metavar "SCRIPT")) <|>
    ((flag' RunInteractiveOption $ long "interactive" <> short 'i') <*> optSyncFlag <*> optDataFlag) <|>
    (flag' PredefinedDocOption $ long "doc-predefined") <|>
    (flag' InfixDocOption $ long "doc-infix") <|>
    ((flag' DumpTableOption $ long "dump-table") <*> optDataFlag)

getOptions :: IO Options
getOptions = execParser (info optParser mempty)
