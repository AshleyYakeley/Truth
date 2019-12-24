module Truth.Core.UI.Specifier.Calendar where

import Data.Time
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data CalendarUISpec sel where
    MkCalendarUISpec :: Subscriber (WholeUpdate Day) -> CalendarUISpec sel

instance Show (CalendarUISpec sel) where
    show (MkCalendarUISpec _) = "calendar"

instance UIType CalendarUISpec where
    uiWitness = $(iowitness [t|CalendarUISpec|])

calendarUISpec :: Subscriber (WholeUpdate Day) -> UISpec sel
calendarUISpec sub = MkUISpec $ MkCalendarUISpec sub
