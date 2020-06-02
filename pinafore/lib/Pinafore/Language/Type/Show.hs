module Pinafore.Language.Type.Show where

import qualified Data.List as List
import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Name
import Prelude (Bounded(..))
import Shapes

class ExprShow t where
    exprShowPrec :: t -> (Text, Int)

precShow :: Int -> (Text, Int) -> Text
precShow c (s, p)
    | c < p = "(" <> s <> ")"
precShow _ (s, _) = s

exprPrecShow :: ExprShow t => Int -> t -> Text
exprPrecShow c t = precShow c $ exprShowPrec t

exprShow :: ExprShow t => t -> Text
exprShow = exprPrecShow maxBound

instance ExprShow Name where
    exprShowPrec (MkName n) = (n, 0)

instance AllWitnessConstraint ExprShow w => ExprShow (AnyInKind w) where
    exprShowPrec (MkAnyInKind (wt :: w t)) =
        case allWitnessConstraint @_ @_ @ExprShow @w @t of
            Dict -> exprShowPrec wt

type family ListTypeExprShow (dv :: [k]) :: Type where
    ListTypeExprShow '[] = (Text, Int)
    ListTypeExprShow (t ': tt) = (Text, Int) -> ListTypeExprShow tt

class GroundExprShow (ground :: GroundTypeKind) where
    groundTypeShowPrec ::
           forall w polarity dv f t.
           ( Is PolarityType polarity
           , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
           , forall a. ExprShow (RangeType w polarity a)
           )
        => ground dv f
        -> DolanArguments dv w f polarity t
        -> (Text, Int)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity) t. (GroundExprShow ground, Is PolarityType polarity) =>
             ExprShow (DolanSingularType ground polarity t) where
    exprShowPrec (VarDolanSingularType namewit) = (pack $ show namewit, 0)
    exprShowPrec (GroundDolanSingularType gt args) = groundTypeShowPrec gt args

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
