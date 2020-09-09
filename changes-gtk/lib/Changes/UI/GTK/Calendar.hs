module Truth.UI.GTK.Calendar
    ( createCalendar
    ) where

import Data.Time
import GI.Gtk as Gtk
import Shapes hiding (get)
import Truth.Core
import Truth.UI.GTK.Useful

createCalendar :: Model (WholeUpdate Day) -> CreateView Widget
createCalendar rmod = do
    esrc <- newEditSource
    widget <- cvNew Calendar []
    let
        getDay ::
               forall m. MonadIO m
            => m Day
        getDay = do
            y <- get widget #year
            m <- get widget #month
            d <- get widget #day
            return $ fromGregorian (toInteger y) (fromIntegral m + 1) (fromIntegral d)
        putDay ::
               forall m. MonadIO m
            => Day
            -> m ()
        putDay day = let
            (y, m, d) = toGregorian day
            in set widget [#year := fromInteger y, #month := fromIntegral m - 1, #day := fromIntegral d]
        onChanged =
            viewRunResource rmod $ \asub -> do
                st <- getDay
                _ <- pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
                return ()
    sig1 <- cvOn widget #daySelected onChanged
    sig2 <- cvOn widget #monthChanged onChanged
    cvBindWholeModel rmod (Just esrc) $ \newval -> do
        oldval <- getDay
        if oldval == newval
            then return ()
            else withSignalsBlocked widget [sig1, sig2] $ putDay newval
    toWidget widget
