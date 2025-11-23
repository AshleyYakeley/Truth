module Pinafore.Language.Convert.HasType.Ground where

import Language.Expression.Dolan

import Import
import Pinafore.Language.Type

type HetCCRVariancesOf :: forall k. k -> CCRVariances
type family HetCCRVariancesOf f where
    HetCCRVariancesOf (_ :: Type) = '[]
    HetCCRVariancesOf (f :: Type -> _) = 'SimpleCCRVariance (VarianceOf f) ': HetCCRVariancesOf (f ())
    HetCCRVariancesOf (f :: (Type, Type) -> _) = 'RangeCCRVariance ': HetCCRVariancesOf (f '((), ()))

type HasQGroundType :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Constraint
class
    (CoercibleKind (CCRVariancesKind dv), dv ~ HetCCRVariancesOf f, HasCCRVariances dv f) =>
    HasQGroundType dv f
        | f -> dv
    where
    qGroundType :: QGroundType dv f

qSomeGroundType ::
    forall dv (t :: CCRVariancesKind dv).
    HasQGroundType dv t =>
    QSomeGroundType
qSomeGroundType = MkSomeGroundType $ qGroundType @dv @t
