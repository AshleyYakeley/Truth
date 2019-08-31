module Truth.UI.GTK.Calendar
    ( calendarGetView
    ) where

import Data.Time
import GI.Gtk as Gtk
import Shapes hiding (get)
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

calendarGetView :: GetGView
calendarGetView =
    MkGetView $ \_ uispec ->
        fmap
            (\MkCalendarUISpec -> do
                 esrc <- newEditSource
                 initial <- cvLiftView $ viewObjectRead $ \_ -> mutableReadToSubject
                 widget <- new Calendar []
                 let
                     getDay ::
                            forall m. MonadIO m
                         => m Day
                     getDay = do
                         y <- get widget #year
                         m <- get widget #month
                         d <- get widget #day
                         return $ fromGregorian (toInteger y) (fromIntegral m) (fromIntegral d)
                     putDay ::
                            forall m. MonadIO m
                         => Day
                         -> m ()
                     putDay day = let
                         (y, m, d) = toGregorian day
                         in set widget [#year := fromInteger y, #month := fromIntegral m, #day := fromIntegral d]
                     onChanged =
                         viewObjectPushEdit $ \_ push -> do
                             st <- getDay
                             _ <- push esrc [MkWholeReaderEdit st]
                             return ()
                 putDay initial
                 _ <- cvLiftView $ viewOn widget #daySelected onChanged
                 _ <- cvLiftView $ viewOn widget #monthChanged onChanged
                 cvReceiveUpdate (Just esrc) $ \_ _ (MkWholeReaderUpdate newval) ->
                     liftIO $ do
                         oldval <- getDay
                         if oldval == newval
                             then return ()
                             else putDay newval
                 toWidget widget) $
        isUISpec uispec
