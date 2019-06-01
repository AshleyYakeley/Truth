module Truth.Core.UI.Specifier.Calendar where

import Data.Time
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data CalendarUISpec sel edit where
    MkCalendarUISpec :: CalendarUISpec sel (WholeEdit Day)

instance Show (CalendarUISpec sel edit) where
    show MkCalendarUISpec = "calendar"

instance UIType CalendarUISpec where
    uiWitness = $(iowitness [t|CalendarUISpec|])

calendarUISpec :: UISpec sel (WholeEdit Day)
calendarUISpec = MkUISpec MkCalendarUISpec
