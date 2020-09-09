module Changes.Core.Types.Unit where

import Changes.Core.Types.None
import Changes.Core.Types.Whole

type ConstWholeEdit a = ConstEdit (WholeReader a)

type ConstWholeUpdate a = ConstUpdate (WholeReader a)

type UnitReader = WholeReader ()

type UnitEdit = ConstWholeEdit ()

type UnitUpdate = ConstWholeUpdate ()
