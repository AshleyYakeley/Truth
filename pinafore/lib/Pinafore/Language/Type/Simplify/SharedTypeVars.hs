module Pinafore.Language.Type.Simplify.SharedTypeVars
    ( mergeSharedTypeVars
    ) where

import Language.Expression.Dolan
import Language.Expression.UVar
import Pinafore.Language.Type.Bisubstitute
import Pinafore.Language.Type.Simplify.VarUses
import Pinafore.Language.Type.Type
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
       forall baseedit a. TypeMappable (PinaforeType baseedit) a
    => a
    -> a
mergeSharedTypeVars expr = let
    (posuses, neguses) = mappableGetVarUses @baseedit expr
    in case findShare posuses <|> findShare neguses of
           Just (MkAnyW (va :: SymbolWitness na), MkAnyW (vb :: SymbolWitness nb)) -> let
               varBij :: Bijection (UVar na) (UVar nb)
               varBij = unsafeUVarBijection
               bisub =
                   MkBisubstitution
                       vb
                       (return $
                        contramap (biBackwards varBij) $ singlePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType va)
                       (return $ fmap (biForwards varBij) $ singlePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType va)
               in mergeSharedTypeVars $ runIdentity $ bisubstitutes [bisub] expr
           Nothing -> expr
