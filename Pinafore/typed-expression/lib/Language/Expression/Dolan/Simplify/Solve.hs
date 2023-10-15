module Language.Expression.Dolan.Simplify.Solve
    ( solveSimplify
    ) where

import Data.Shim
import Language.Expression.Dolan.Simplify.AutomateRecursion
import Language.Expression.Dolan.Simplify.Safety
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

simplifySafetyINTERNAL :: Bool
simplifySafetyINTERNAL = True

solveSimplify ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanTypeCheckM ground (DolanShimWit ground polarity t)
solveSimplify t =
    unEndoM
        (mconcat
             [ mif simplifySafetyINTERNAL $ MkEndoM $ chainShimWitM $ checkSafetyInType "unify before"
             , MkEndoM $ chainShimWitM automateRecursionInType
             , mif simplifySafetyINTERNAL $ MkEndoM $ chainShimWitM $ checkSafetyInType "unify after"
             ]) $
    mkShimWit t
