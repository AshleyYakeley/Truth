module Pinafore.Language.Interpret.TypeDecl.DoubleParams
    ( RangeVars
    , DoubleParams(..)
    , doubleParameterNames
    , doubleTypeParameters
    ) where

import Import
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

data RangeVars t = MkRangeVars
    { coVar :: TypeVarT t
    , _contraVar :: TypeVarT t
    }

swapRangeVars :: RangeVars --> RangeVars
swapRangeVars (MkRangeVars a b) = MkRangeVars b a

doubleParameterVars :: TypeVarT --> RangeVars
doubleParameterVars (typeVarName -> n) = MkRangeVars (newAssignTypeVar $ n <> "+") (newAssignTypeVar $ n <> "-")

-- | contra, co
doubleParameterNames :: Name -> (Name, Name)
doubleParameterNames (MkName n) = (MkName $ n <> "-", MkName $ n <> "+")

noRangeVars :: TypeVarT --> RangeVars
noRangeVars v = MkRangeVars v v

class DoubleParams t where
    rangeMapParams :: (TypeVarT --> RangeVars) -> t -> t

doubleTypeParameters :: DoubleParams t => [Name] -> t -> t
doubleTypeParameters [] = id
doubleTypeParameters nn =
    rangeMapParams $ \v ->
        if elem (typeVarToName v) nn
            then doubleParameterVars v
            else noRangeVars v

instance DoubleParams t => DoubleParams (FixedList n t) where
    rangeMapParams vv = fmap $ rangeMapParams vv

instance (forall a. DoubleParams (w a)) => DoubleParams (ListVType (w :: Type -> Type) tt) where
    rangeMapParams vv = mapListVType $ rangeMapParams vv

instance (forall a. DoubleParams (w a)) => DoubleParams (SomeFor f w) where
    rangeMapParams vv (MkSomeFor wa fa) = MkSomeFor (rangeMapParams vv wa) fa

instance forall (w :: CCRArgumentKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv) (t :: Type). (forall (sv :: CCRVariance) (a :: CCRVarianceKind sv).
                                                                                                              DoubleParams (w sv a)) =>
             DoubleParams (CCRArguments w dv gt t) where
    rangeMapParams _ NilCCRArguments = NilCCRArguments
    rangeMapParams vv (ConsCCRArguments arg1 argr) = ConsCCRArguments (rangeMapParams vv arg1) (rangeMapParams vv argr)

instance DoubleParams (CCRPolarArgument QType polarity sv a) where
    rangeMapParams vv (CoCCRPolarArgument q) = CoCCRPolarArgument $ rangeMapParams vv q
    rangeMapParams vv (ContraCCRPolarArgument p) = ContraCCRPolarArgument $ rangeMapParams (fmap swapRangeVars vv) p
    rangeMapParams vv (RangeCCRPolarArgument p q) =
        RangeCCRPolarArgument (rangeMapParams (fmap swapRangeVars vv) p) (rangeMapParams vv q)

instance DoubleParams (QGroundedType polarity t) where
    rangeMapParams vv (MkDolanGroundedType gt args) = MkDolanGroundedType gt $ rangeMapParams vv args

instance DoubleParams (QSingularType polarity t) where
    rangeMapParams vv (GroundedDolanSingularType t) = GroundedDolanSingularType $ rangeMapParams vv t
    rangeMapParams vv (VarDolanSingularType v) = VarDolanSingularType $ coVar $ vv v
    rangeMapParams vv (RecursiveDolanSingularType rv t) = let
        vv' :: TypeVarT --> RangeVars
        vv' v =
            if isJust $ testEquality rv v
                then noRangeVars v
                else vv v
        in RecursiveDolanSingularType rv $ rangeMapParams vv' t

instance DoubleParams (QType polarity t) where
    rangeMapParams _ NilDolanType = NilDolanType
    rangeMapParams vv (ConsDolanType t1 tr) = ConsDolanType (rangeMapParams vv t1) (rangeMapParams vv tr)

instance DoubleParams (NonpolarArgument QGroundType sv a) where
    rangeMapParams vv (CoNonpolarArgument q) = CoNonpolarArgument $ rangeMapParams vv q
    rangeMapParams vv (ContraNonpolarArgument p) = ContraNonpolarArgument $ rangeMapParams (fmap swapRangeVars vv) p
    rangeMapParams vv (RangeNonpolarArgument p q) =
        RangeNonpolarArgument (rangeMapParams (fmap swapRangeVars vv) p) (rangeMapParams vv q)

instance DoubleParams (NonpolarType QGroundType t) where
    rangeMapParams vv (GroundedNonpolarType gt args) = GroundedNonpolarType gt $ rangeMapParams vv args
    rangeMapParams vv (VarNonpolarType v) = VarNonpolarType $ coVar $ vv v
    rangeMapParams vv (RecursiveNonpolarType rv t) = let
        vv' :: TypeVarT --> RangeVars
        vv' v =
            if isJust $ testEquality rv v
                then noRangeVars v
                else vv v
        in RecursiveNonpolarType rv $ rangeMapParams vv' t

instance DoubleParams (QSignature 'Positive t) where
    rangeMapParams vv (ValueSignature mft n t mexpr) = ValueSignature mft n (rangeMapParams vv t) mexpr
