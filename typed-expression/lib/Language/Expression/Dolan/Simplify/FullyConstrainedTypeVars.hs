module Language.Expression.Dolan.Simplify.FullyConstrainedTypeVars
    ( fullyConstrainedTypeVars
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Inverted
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Simplify.VarUses
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

eliminationBisubs ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => (FiniteSet (AnyW SymbolType), FiniteSet (AnyW SymbolType))
    -> [Bisubstitution ground (DolanPolyShim ground Type) Identity]
eliminationBisubs (posvars, negvars) = let
    posbisub :: AnyW SymbolType -> Bisubstitution ground (DolanPolyShim ground Type) Identity
    posbisub (MkAnyW var) =
        assignUVar @Type @BottomType var $
        MkBisubstitution False var (return $ mkShimWit NilDolanType) (return $ varDolanShimWit var)
    negbisub :: AnyW SymbolType -> Bisubstitution ground (DolanPolyShim ground Type) Identity
    negbisub (MkAnyW var) =
        assignUVar @Type @TopType var $
        MkBisubstitution False var (return $ varDolanShimWit var) (return $ mkShimWit NilDolanType)
    in (fmap posbisub $ toList posvars) <> (fmap negbisub $ toList negvars)

eliminateVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => (FiniteSet (AnyW SymbolType), FiniteSet (AnyW SymbolType))
    -> a
    -> a
eliminateVars vars expr = runIdentity $ bisubstitutes @ground (eliminationBisubs vars) expr

testInvertedSubtype ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground
    => AnyW (DolanType ground 'Negative)
    -> AnyW (DolanType ground 'Positive)
    -> DolanM ground Bool
testInvertedSubtype (MkAnyW negtype) (MkAnyW postype) = do
    mexpr <- mcatch $ runVarRenamerT $ runSolver $ invertedPolarSubtype @ground negtype postype
    return $
        case mexpr of
            Nothing -> False
            Just expr -> isClosedExpression expr

fullyConstrainedTypeVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> DolanTypeCheckM ground a
fullyConstrainedTypeVars expr =
    liftTypeCheck $ do
        let
            (setFromList -> posvars, setFromList -> negvars) = mappableGetVars @ground expr
            allvars = union posvars negvars
            (posapprs, negapprs) = mappableGetAppearances @ground expr
            testElimVar :: AnyW SymbolType -> DolanM ground Bool
            testElimVar var = let
                postypes = appearanceMatchingTypes @ground var posapprs
                negtypes = appearanceMatchingTypes @ground var negapprs
                in fmap (and . fmap and) $ for negtypes $ \nt -> for postypes $ \pt -> testInvertedSubtype nt pt
        fcvars <- forfilt allvars testElimVar
        return $ eliminateVars @ground (fcvars, fcvars) expr
