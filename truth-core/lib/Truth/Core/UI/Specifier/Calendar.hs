module Truth.Core.UI.Specifier.Calendar where

import Data.Time
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data CalendarUISpec sel update where
    MkCalendarUISpec :: CalendarUISpec sel (WholeUpdate Day)

instance Show (CalendarUISpec sel update) where
    show MkCalendarUISpec = "calendar"

instance UIType CalendarUISpec where
    uiWitness = $(iowitness [t|CalendarUISpec|])

calendarUISpec :: UISpec sel (WholeUpdate Day)
calendarUISpec = MkUISpec MkCalendarUISpec
