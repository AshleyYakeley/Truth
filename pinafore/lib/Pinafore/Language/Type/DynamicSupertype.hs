module Pinafore.Language.Type.DynamicSupertype where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Shim
import Pinafore.Language.Type.Subtype ()
import Pinafore.Language.Type.Type
import Shapes

data GreatestDynamicSupertype t =
    forall dt. MkGreatestDynamicSupertype (PinaforeShimWit 'Negative dt)
                                          (PinaforePolyShim Type t dt)
                                          (PinaforePolyShim Type dt (Maybe t))

getGreatestDynamicSupertype :: PinaforeType 'Positive t -> PinaforeSourceScoped (GreatestDynamicSupertype t)
getGreatestDynamicSupertype t = do
    t' <- invertType t
    return $ MkGreatestDynamicSupertype t' id $ functionToShim "dynamic-supertype" Just
