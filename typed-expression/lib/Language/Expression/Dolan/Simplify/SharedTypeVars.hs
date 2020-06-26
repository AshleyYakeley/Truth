module Language.Expression.Dolan.Simplify.SharedTypeVars
    ( mergeSharedTypeVars
    ) where

import Data.Shim
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Simplify.VarUses
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeVariable
import Shapes

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

mergeSharedTypeVars ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a => a -> a
mergeSharedTypeVars expr = let
    (posuses, neguses) = mappableGetVarUses @ground expr
    in case findShare posuses <|> findShare neguses of
           Just (MkAnyW (va :: SymbolType na), MkAnyW (vb :: SymbolType nb)) ->
               assignUVar @Type @(UVar Type na) vb $ let
                   bisub =
                       MkBisubstitution
                           vb
                           (return $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType va)
                           (return $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType va)
                   in mergeSharedTypeVars @ground $ runIdentity $ bisubstitutes @ground [bisub] expr
           Nothing -> expr
