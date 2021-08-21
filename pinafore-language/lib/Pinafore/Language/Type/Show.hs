{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type.Show where

import qualified Data.List as List
import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.ExprShow
import Shapes

type GroundExprShow :: GroundTypeKind -> Constraint
class GroundExprShow ground where
    groundTypeShowPrec ::
           forall (w :: Polarity -> Type -> Type) polarity dv f t.
           ( Is PolarityType polarity
           , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
           , forall a polarity'. Is PolarityType polarity' => ExprShow (RangeType w polarity' a)
           )
        => ground dv f
        -> DolanArguments dv w f polarity t
        -> (Text, Int)

saturatedGroundTypeShowPrec ::
       forall (ground :: GroundTypeKind) (w :: Polarity -> Type -> Type) dv f.
       ( IsDolanGroundType ground
       , GroundExprShow ground
       , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
       , forall a polarity'. Is PolarityType polarity' => ExprShow (RangeType w polarity' a)
       )
    => (forall pol. Is PolarityType pol => AnyW (w pol))
    -> ground dv f
    -> (Text, Int)
saturatedGroundTypeShowPrec avar gt = let
    singleVarArgument ::
           forall polarity sv r. Is PolarityType polarity
        => CCRVarianceType sv
        -> (forall a. InKind a => SingleArgument sv w polarity a -> r)
        -> r
    singleVarArgument CoCCRVarianceType call =
        case avar of
            MkAnyW var -> call var
    singleVarArgument ContraCCRVarianceType call =
        invertPolarity @polarity $
        case avar of
            MkAnyW var -> call var
    singleVarArgument RangeCCRVarianceType call =
        invertPolarity @polarity $
        case (avar, avar) of
            (MkAnyW var1, MkAnyW var2) -> call $ MkRangeType var1 var2
    allVarArguments ::
           forall polarity dv' f' r. Is PolarityType polarity
        => DolanVarianceType dv'
        -> (forall t. DolanArguments dv' w f' polarity t -> r)
        -> r
    allVarArguments NilListType call = call NilDolanArguments
    allVarArguments (ConsListType svt dvt) call =
        singleVarArgument @polarity svt $ \arg -> allVarArguments dvt $ \args -> call $ ConsDolanArguments arg args
    in allVarArguments @'Positive (groundTypeVarianceType gt) $ \args -> groundTypeShowPrec gt args

instance forall (ground :: GroundTypeKind) (polarity :: Polarity) t. (GroundExprShow ground, Is PolarityType polarity) =>
             ExprShow (DolanSingularType ground polarity t) where
    exprShowPrec (VarDolanSingularType namewit) = exprShowPrec namewit
    exprShowPrec (GroundedDolanSingularType gt args) = groundTypeShowPrec gt args
    exprShowPrec (RecursiveDolanSingularType n pt) = ("rec " <> exprShow n <> ". " <> exprShow pt, 4)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity) t. (GroundExprShow ground, Is PolarityType polarity) =>
             ExprShow (DolanType ground polarity t) where
    exprShowPrec NilDolanType =
        case polarityType @polarity of
            PositiveType -> ("None", 0)
            NegativeType -> ("Any", 0)
    exprShowPrec (ConsDolanType ta NilDolanType) = exprShowPrec ta
    exprShowPrec (ConsDolanType ta tb) = let
        jmConnector =
            case polarityType @polarity of
                PositiveType -> " | "
                NegativeType -> " & "
        in (exprPrecShow 2 ta <> jmConnector <> exprPrecShow 2 tb, 3)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (GroundExprShow ground, Is PolarityType polarity) =>
             AllWitnessConstraint ExprShow (DolanType ground polarity) where
    allWitnessConstraint = Dict

instance forall (ground :: GroundTypeKind) (polarity :: Polarity) t. (GroundExprShow ground, Is PolarityType polarity) =>
             ExprShow (RangeType (DolanType ground) polarity t) where
    exprShowPrec (MkRangeType t1 t2) = let
        getpieces ::
               forall pol a. Is PolarityType pol
            => DolanType ground pol a
            -> [Text]
        getpieces NilDolanType = []
        getpieces (ConsDolanType t tr) = exprPrecShow 0 t : getpieces tr
        contrapieces = nub $ invertPolarity @polarity $ getpieces t1
        copieces = nub $ getpieces t2
        bothpieces = List.intersect contrapieces copieces
        rcontrapieces = contrapieces List.\\ bothpieces
        rcopieces = copieces List.\\ bothpieces
        pieces :: [Text]
        pieces = bothpieces <> fmap ("-" <>) rcontrapieces <> fmap ("+" <>) rcopieces
        text :: Text
        text =
            case pieces of
                [t] -> t
                _ -> "{" <> ointercalate "," pieces <> "}"
        in (text, 0)
