module Truth.Core.Types.Unit where

import Truth.Core.Types.None
import Truth.Core.Types.Whole

type ConstWholeEdit a = ConstEdit (WholeReader a)

type ConstWholeUpdate a = ConstUpdate (WholeReader a)

type UnitReader = WholeReader ()

type UnitEdit = ConstWholeEdit ()

type UnitUpdate = ConstWholeUpdate ()
