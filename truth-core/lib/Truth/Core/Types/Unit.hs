module Truth.Core.Types.Unit where

import Truth.Core.Edit
import Truth.Core.Types.None
import Truth.Core.Types.Whole

type ConstEdit a = NoEdit (WholeReader a)

type UnitReader = WholeReader ()

type UnitEdit = ConstEdit ()

unitUpdateFunction :: UpdateFunction edit UnitEdit
unitUpdateFunction = constUpdateFunction ()

unitLens :: EditLens edit UnitEdit
unitLens = readOnlyEditLens unitUpdateFunction
