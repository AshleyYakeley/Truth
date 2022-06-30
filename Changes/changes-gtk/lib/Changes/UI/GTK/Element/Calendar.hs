module Changes.UI.GTK.Element.Calendar
    ( createCalendar
    ) where

import Changes.Core
import Changes.GI
import Data.Time
import GI.Gtk as Gtk
import Shapes hiding (get)

createCalendar :: Model (WholeUpdate Day) -> GView 'Locked Widget
createCalendar rmod = do
    esrc <- newEditSource
    widget <- gvNew Calendar []
    let
        getDay :: GView 'Locked Day
        getDay = do
            y <- get widget #year
            m <- get widget #month
            d <- get widget #day
            return $ fromGregorian (toInteger y) (succ $ fromIntegral m) (fromIntegral d)
        putDay :: Day -> GView 'Locked ()
        putDay day = let
            (y, m, d) = toGregorian day
            in set widget [#year := fromInteger y, #month := pred (fromIntegral m), #day := fromIntegral d]
        onChanged :: GView 'Locked ()
        onChanged = do
            st <- getDay
            gvRunResource rmod $ \asub -> do
                _ <- pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
                return ()
    sig1 <- gvOnSignal widget #daySelected onChanged
    sig2 <- gvOnSignal widget #monthChanged onChanged
    gvBindWholeModel rmod (Just esrc) $ \newval -> do
        oldval <- getDay
        if oldval == newval
            then return ()
            else withSignalsBlocked widget [sig1, sig2] $ putDay newval
    toWidget widget
