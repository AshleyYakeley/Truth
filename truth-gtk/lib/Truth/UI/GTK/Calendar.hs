module Truth.UI.GTK.Calendar
    ( calendarGetView
    ) where

import Data.Time
import GI.Gtk as Gtk
import Shapes hiding (get)
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful
import Truth.Debug.Reference

calendarGetView :: GetGView
calendarGetView =
    MkGetView $ \_ uispec ->
        fmap
            (\(MkCalendarUISpec rmod) -> traceBracket "GTK.Calendar:create" $ do
                 esrc <- newEditSource
                 widget <- new Calendar []
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
                         in traceBracket "GTK.Calendar:set" $ set widget [#year := fromInteger y, #month := fromIntegral m - 1, #day := fromIntegral d]
                     onChanged = traceBracket "GTK.Calendar:changed" $
                         viewRunResource rmod $ \asub -> do
                             st <- getDay
                             _ <- pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
                             return ()
                 sig1 <- cvOn widget #daySelected onChanged
                 sig2 <- cvOn widget #monthChanged onChanged
                 cvBindWholeModel rmod (Just esrc) $ \newval -> traceBracket "GTK.Calendar:update" $ do
                     oldval <- getDay
                     if oldval == newval
                         then return ()
                         else liftIO $ withSignalBlocked widget sig1 $ withSignalBlocked widget sig2 $ putDay newval
                 toWidget widget) $
        isUISpec uispec
