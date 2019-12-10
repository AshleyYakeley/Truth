module Truth.Core.Types.Unit where

import Truth.Core.Edit
import Truth.Core.Types.None
import Truth.Core.Types.Whole

type ConstEdit a = NoEdit (WholeReader a)

type ConstUpdate a = NoUpdate (WholeReader a)

type UnitReader = WholeReader ()

type UnitEdit = ConstEdit ()

type UnitUpdate = ConstUpdate ()

unitUpdateFunction :: UpdateFunction edit UnitUpdate
unitUpdateFunction = constUpdateFunction ()

unitLens :: EditLens edit UnitUpdate
unitLens = readOnlyEditLens unitUpdateFunction
