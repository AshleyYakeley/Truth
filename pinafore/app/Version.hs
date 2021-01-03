module Version
    ( printVersion
    ) where

import Data.Time
import GitHash
import Shapes

pinaforeVersion :: String
pinaforeVersion = "0.3"

gi :: GitInfo
gi = $$tGitInfoCwd

commitZonedTime :: ZonedTime
commitZonedTime = parseTimeOrError True defaultTimeLocale "%a %b %-e %T %Y %z" (giCommitDate gi)

commitTimeString :: String
commitTimeString = formatTime defaultTimeLocale "%FT%TZ" $ zonedTimeToUTC commitZonedTime

printVersion :: IO ()
printVersion =
    putStrLn $
    "Pinafore version " <>
    pinaforeVersion <>
    " (" <>
    commitTimeString <>
    " " <>
    giHash gi <>
    ")" <>
    if giDirty gi
        then "+"
        else ""
