module Truth.Core.Types.Unit where

import Truth.Core.Edit
import Truth.Core.Types.None
import Truth.Core.Types.Whole

type ConstEdit a = NoEdit (WholeReader a)

type UnitEdit = ConstEdit ()

unitEditFunction :: PureEditFunction edit UnitEdit
unitEditFunction = constEditFunction ()

unitLens :: PureEditLens edit UnitEdit
unitLens = readOnlyEditLens unitEditFunction
