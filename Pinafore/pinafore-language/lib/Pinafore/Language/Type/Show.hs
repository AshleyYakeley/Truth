{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type.Show where

import qualified Data.List as List
import Data.Shim
import Language.Expression.Common
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
    => (forall pol. Is PolarityType pol => Some (w pol))
    -> ground dv f
    -> (Text, Int)
saturatedGroundTypeShowPrec avar gt = let
    singleVarArgument ::
           forall polarity sv r. Is PolarityType polarity
        => CCRVarianceType sv
        -> (forall a. CCRPolarArgument w polarity sv a -> r)
        -> r
    singleVarArgument CoCCRVarianceType call =
        case avar of
            MkSome var -> call $ CoCCRPolarArgument var
    singleVarArgument ContraCCRVarianceType call =
        invertPolarity @polarity $
        case avar of
            MkSome var -> call $ ContraCCRPolarArgument var
    singleVarArgument RangeCCRVarianceType call =
        invertPolarity @polarity $
        case (avar, avar) of
            (MkSome var1, MkSome var2) -> call $ RangeCCRPolarArgument var1 var2
    allVarArguments ::
           forall polarity dv' f' r. Is PolarityType polarity
        => DolanVarianceType dv'
        -> (forall t. DolanArguments dv' w f' polarity t -> r)
        -> r
    allVarArguments NilListType call = call NilCCRArguments
    allVarArguments (ConsListType svt dvt) call =
        singleVarArgument @polarity svt $ \arg -> allVarArguments dvt $ \args -> call $ ConsCCRArguments arg args
    in allVarArguments @'Positive (groundTypeVarianceType gt) $ \args -> groundTypeShowPrec gt args

showGroundType ::
       forall (ground :: GroundTypeKind) dv gt. (IsDolanGroundType ground, GroundExprShow ground)
    => ground dv gt
    -> Text
showGroundType t =
    newUVar "_" $ \var ->
        fst $ saturatedGroundTypeShowPrec @ground (MkSome $ singleDolanType @ground $ VarDolanSingularType var) t

instance forall (ground :: GroundTypeKind) (polarity :: Polarity) t. (GroundExprShow ground, Is PolarityType polarity) =>
             ExprShow (DolanGroundedType ground polarity t) where
    exprShowPrec (MkDolanGroundedType gt args) = groundTypeShowPrec gt args

instance forall (ground :: GroundTypeKind) (polarity :: Polarity) t. (GroundExprShow ground, Is PolarityType polarity) =>
             ExprShow (DolanSingularType ground polarity t) where
    exprShowPrec (VarDolanSingularType namewit) = exprShowPrec namewit
    exprShowPrec (GroundedDolanSingularType t) = exprShowPrec t
    exprShowPrec (RecursiveDolanSingularType n pt) = ("rec " <> exprShow n <> ". " <> exprPrecShow 7 pt, 7)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (GroundExprShow ground, Is PolarityType polarity) =>
             AllConstraint ExprShow (DolanGroundedType ground polarity) where
    allConstraint = Dict

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
        in (exprPrecShow 6 ta <> jmConnector <> exprPrecShow 6 tb, 7)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (GroundExprShow ground, Is PolarityType polarity) =>
             AllConstraint ExprShow (DolanType ground polarity) where
    allConstraint = Dict

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

instance forall (ground :: GroundTypeKind) t. (IsDolanGroundType ground, GroundExprShow ground) =>
             ExprShow (NonpolarDolanType ground t) where
    exprShowPrec npt =
        case nonpolarToDolanType @ground @'Positive npt of
            MkShimWit pt _ -> exprShowPrec pt

instance forall (ground :: GroundTypeKind). (IsDolanGroundType ground, GroundExprShow ground) =>
             AllConstraint ExprShow (NonpolarDolanType ground) where
    allConstraint = Dict
