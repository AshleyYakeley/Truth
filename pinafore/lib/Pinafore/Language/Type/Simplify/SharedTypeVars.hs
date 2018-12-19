module Pinafore.Language.Type.Simplify.SharedTypeVars
    ( mergeSharedTypeVarsInType
    , mergeSharedTypeVarsInExpression
    ) where

import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Sealed
import Language.Expression.UVar
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Pinafore.Language.Type.Unify
import Shapes

class GetExpressionVarUses t where
    -- | (positive, negative)
    getExpressionVarUses :: t -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])

instance IsTypePolarity polarity => GetExpressionVarUses (RangeType (PinaforeType baseedit) polarity a) where
    getExpressionVarUses (MkRangeType tp tq) =
        invertPolarity @polarity $ getExpressionVarUses tp <> getExpressionVarUses tq

getArgExpressionVarUses ::
       forall baseedit polarity sv a. IsTypePolarity polarity
    => SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) polarity a
    -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])
getArgExpressionVarUses CovarianceType t = getExpressionVarUses t
getArgExpressionVarUses ContravarianceType t = invertPolarity @polarity $ getExpressionVarUses t
getArgExpressionVarUses RangevarianceType t = getExpressionVarUses t

getArgsExpressionVarUses ::
       forall baseedit polarity dv gt t. IsTypePolarity polarity
    => DolanVarianceType dv
    -> DolanArguments dv (PinaforeType baseedit) gt polarity t
    -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])
getArgsExpressionVarUses NilListType NilDolanArguments = mempty
getArgsExpressionVarUses (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVarUses @baseedit @polarity sv arg <> getArgsExpressionVarUses dv args

instance IsTypePolarity polarity => GetExpressionVarUses (PinaforeSingularType baseedit polarity t) where
    getExpressionVarUses (GroundPinaforeSingularType gt args) =
        getArgsExpressionVarUses (pinaforeGroundTypeKind gt) args
    getExpressionVarUses (VarPinaforeSingularType _) = mempty

getExpressionVarUses' ::
       IsTypePolarity polarity => PinaforeType baseedit polarity t -> ([[AnyW SymbolWitness]], [[AnyW SymbolWitness]])
getExpressionVarUses' NilPinaforeType = mempty
getExpressionVarUses' (ConsPinaforeType t1 tr) = getExpressionVarUses t1 <> getExpressionVarUses' tr

getJMSingleTypeVars :: IsTypePolarity polarity => PinaforeSingularType baseedit polarity t -> [AnyW SymbolWitness]
getJMSingleTypeVars (VarPinaforeSingularType vn) = [MkAnyW vn]
getJMSingleTypeVars (GroundPinaforeSingularType _ _) = []

getJMTypeVars :: IsTypePolarity polarity => PinaforeType baseedit polarity t -> [AnyW SymbolWitness]
getJMTypeVars NilPinaforeType = mempty
getJMTypeVars (ConsPinaforeType t1 tr) = getJMSingleTypeVars t1 <> getJMTypeVars tr

instance IsTypePolarity polarity => GetExpressionVarUses (PinaforeType baseedit polarity t) where
    getExpressionVarUses t =
        case getJMTypeVars t of
            tv ->
                (case whichTypePolarity @polarity of
                     Left Refl -> ([tv], [])
                     Right Refl -> ([], [tv])) <>
                getExpressionVarUses' t

instance GetExpressionVarUses (NamedExpression name (PinaforeType baseedit 'NegativePolarity) t) where
    getExpressionVarUses (ClosedExpression _) = mempty
    getExpressionVarUses (OpenExpression (MkNameWitness _ t) expr) = getExpressionVarUses t <> getExpressionVarUses expr

instance GetExpressionVarUses (PinaforeExpression baseedit) where
    getExpressionVarUses (MkSealedExpression twt expr) = getExpressionVarUses twt <> getExpressionVarUses expr

findShare ::
       forall a. Eq a
    => [[a]]
    -> Maybe (a, a)
findShare uses = let
    allvars = nub $ mconcat uses
    getpairs :: [a] -> [(a, a)]
    getpairs [] = []
    getpairs (v:vv) = fmap (\v' -> (v', v)) vv <> getpairs vv
    pairs = getpairs allvars
    gooduse :: (a, a) -> [a] -> Bool
    gooduse (a, b) l = elem a l == elem b l
    goodpair :: (a, a) -> Bool
    goodpair pair = all (gooduse pair) uses
    in find goodpair pairs

mergeSharedTypeVarsInType ::
       forall baseedit polarity t. IsTypePolarity polarity
    => PinaforeType baseedit polarity t
    -> PinaforeTypeF baseedit polarity t
mergeSharedTypeVarsInType t = let
    (posuses, neguses) = getExpressionVarUses t
    in case findShare posuses <|> findShare neguses of
           Just (MkAnyW (va :: SymbolWitness na), MkAnyW (vb :: SymbolWitness nb)) -> let
               varBij :: Bijection (UVar na) (UVar nb)
               varBij = unsafeUVarBijection
               bisub =
                   MkBisubstitution
                       vb
                       (contramap (biBackwards varBij) $ singlePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType va)
                       (fmap (biForwards varBij) $ singlePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType va)
               in chainTypeF mergeSharedTypeVarsInType $ bisubstituteType bisub t
           Nothing -> mkTypeF t

mergeSharedTypeVarsInExpression :: forall baseedit. PinaforeExpression baseedit -> PinaforeExpression baseedit
mergeSharedTypeVarsInExpression expr = let
    (posuses, neguses) = getExpressionVarUses expr
    in case findShare posuses <|> findShare neguses of
           Just (MkAnyW (va :: SymbolWitness na), MkAnyW (vb :: SymbolWitness nb)) -> let
               varBij :: Bijection (UVar na) (UVar nb)
               varBij = unsafeUVarBijection
               bisub =
                   MkBisubstitution
                       vb
                       (contramap (biBackwards varBij) $ singlePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType va)
                       (fmap (biForwards varBij) $ singlePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType va)
               in mergeSharedTypeVarsInExpression $ bisubstitutesSealedExpression [bisub] expr
           Nothing -> expr
