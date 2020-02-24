module Truth.Core.UI.Specifier.Calendar where

import Data.Time
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data CalendarUISpec where
    MkCalendarUISpec :: Subscriber (WholeUpdate Day) -> CalendarUISpec

instance Show CalendarUISpec where
    show (MkCalendarUISpec _) = "calendar"

instance UIType CalendarUISpec where
    uiWitness = $(iowitness [t|CalendarUISpec|])

calendarUISpec :: Subscriber (WholeUpdate Day) -> CVUISpec
calendarUISpec sub = mkCVUISpec $ MkCalendarUISpec sub
