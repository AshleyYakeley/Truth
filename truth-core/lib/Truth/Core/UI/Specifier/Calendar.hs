module Truth.Core.UI.Specifier.Calendar where

import Data.Time
import Truth.Core.Import
import Truth.Core.Reference
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data CalendarUISpec where
    MkCalendarUISpec :: Model (WholeUpdate Day) -> CalendarUISpec

instance Show CalendarUISpec where
    show (MkCalendarUISpec _) = "calendar"

instance UIType CalendarUISpec where
    uiWitness = $(iowitness [t|CalendarUISpec|])

calendarUISpec :: Model (WholeUpdate Day) -> CVUISpec
calendarUISpec sub = mkCVUISpec $ MkCalendarUISpec sub
