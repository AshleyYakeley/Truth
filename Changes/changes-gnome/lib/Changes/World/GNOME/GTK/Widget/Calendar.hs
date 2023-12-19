module Changes.World.GNOME.GTK.Widget.Calendar
    ( createCalendar
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import Data.Time
import GI.Gtk as Gtk
import Shapes hiding (get)

createCalendar :: Model (WholeUpdate Day) -> GView 'Unlocked Widget
createCalendar rmod = do
    esrc <- gvNewEditSource
    gvRunLockedThen $ do
        (calendar, widget) <- gvNewWidget Calendar []
        let
            getDay :: GView 'Locked Day
            getDay = do
                y <- get calendar #year
                m <- get calendar #month
                d <- get calendar #day
                return $ fromGregorian (toInteger y) (succ $ fromIntegral m) (fromIntegral d)
            putDay :: Day -> GView 'Locked ()
            putDay day = let
                (y, m, d) = toGregorian day
                in set calendar [#year := fromInteger y, #month := pred (fromIntegral m), #day := fromIntegral d]
            onChanged :: GView 'Locked ()
            onChanged = do
                st <- getDay
                _ <- gvRunUnlocked $ gvSetWholeModel rmod esrc st
                return ()
        sig1 <- gvOnSignal calendar #daySelected onChanged
        sig2 <- gvOnSignal calendar #monthChanged onChanged
        return $ do
            gvBindWholeModel rmod (Just esrc) $ \newval ->
                gvRunLocked $ do
                    oldval <- getDay
                    if oldval == newval
                        then return ()
                        else withSignalsBlocked calendar [sig1, sig2] $ putDay newval
            return widget
