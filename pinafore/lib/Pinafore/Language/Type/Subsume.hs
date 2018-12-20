{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type.Subsume
    (
    ) where

import Pinafore.Language.Type
import Pinafore.Language.Type.Simplify
import Pinafore.Language.Type.Unify
import Shapes

--import Language.Expression.Sealed
import Language.Expression.Subsumer

pinaforeSubsumePosWitnesses ::
       PinaforeType baseedit 'PositivePolarity inf
    -> PinaforeType baseedit 'PositivePolarity decl
    -> PinaforeUnifierMonad baseedit (PinaforeUnifier baseedit (inf -> decl))
pinaforeSubsumePosWitnesses _ _ = return undefined

instance Subsumer (PinaforeUnifier baseedit) where
    subsumePosWitnesses = pinaforeSubsumePosWitnesses
    simplifyPosType (MkAnyW t) =
        case pinaforeSimplifyType t of
            MkTypeF t' _ -> MkAnyW t'
