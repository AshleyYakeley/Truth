{-# LANGUAGE CPP #-}
module Pinafore.Version
    ( printVersion
    ) where

#ifdef FLAG_GITVERSION
import Data.Time
import GitHash
#endif
import Shapes

pinaforeVersion :: String
pinaforeVersion = PINAFOREVERSION

extraVersion :: String
extraVersion =
#ifdef FLAG_GITVERSION
    let
    gi :: GitInfo
    gi = $$tGitInfoCwd
    commitZonedTime :: ZonedTime
    commitZonedTime = parseTimeOrError True defaultTimeLocale "%a %b %-e %T %Y %z" (giCommitDate gi)
    commitTimeString :: String
    commitTimeString = formatTime defaultTimeLocale "%FT%TZ" $ zonedTimeToUTC commitZonedTime
    flag :: String
    flag =
        if giDirty gi
            then "+"
            else ""
    in " (" <> commitTimeString <> " " <> giHash gi <> ")" <> flag
#else
    ""
#endif

printVersion :: IO ()
printVersion = putStrLn $ "Pinafore version " <> pinaforeVersion <> extraVersion
