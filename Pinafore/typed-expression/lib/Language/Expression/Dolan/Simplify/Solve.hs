module Language.Expression.Dolan.Simplify.Solve
    ( solveSimplify
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Simplify.AutomateRecursion
import Language.Expression.Dolan.Simplify.Safety
import Language.Expression.Dolan.SubtypeChain
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem

simplifySafetyINTERNAL :: Bool
simplifySafetyINTERNAL = False

solveSimplify ::
    forall (ground :: GroundTypeKind) polarity t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    DolanType ground polarity t ->
    DolanRenameTypeM ground (DolanShimWit ground polarity t)
solveSimplify t =
    unEndoM
        ( mconcat
            [ mif simplifySafetyINTERNAL $ MkEndoM $ chainShimWitM $ checkSafetyInType "unify before"
            , MkEndoM $ chainShimWitM automateRecursionInType
            , mif simplifySafetyINTERNAL $ MkEndoM $ chainShimWitM $ checkSafetyInType "unify after"
            ]
        )
        $ mkShimWit t
