module Language.Expression.Dolan.Simplify.OneSidedTypeVars
    ( eliminateOneSidedTypeVars
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Simplify.VarUses
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

eliminationBisubs ::
    forall (ground :: GroundTypeKind).
    IsDolanGroundType ground =>
    (ListSet SomeTypeVarT, ListSet SomeTypeVarT) ->
    [Bisubstitution ground (DolanShim ground) Identity]
eliminationBisubs (posvars, negvars) = let
    posbisub :: SomeTypeVarT -> Bisubstitution ground (DolanShim ground) Identity
    posbisub (MkSomeTypeVarT var) =
        assignTypeVarT @BottomType var $ MkBisubstitution var (return nilDolanShimWit) (return $ varDolanShimWit var)
    negbisub :: SomeTypeVarT -> Bisubstitution ground (DolanShim ground) Identity
    negbisub (MkSomeTypeVarT var) =
        assignTypeVarT @TopType var $ MkBisubstitution var (return $ varDolanShimWit var) (return nilDolanShimWit)
    in (fmap posbisub $ toList posvars) <> (fmap negbisub $ toList negvars)

eliminateVars ::
    forall (ground :: GroundTypeKind) a.
    (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a) =>
    (ListSet SomeTypeVarT, ListSet SomeTypeVarT) ->
    Endo a
eliminateVars vars = endoMToEndo $ bisubstitutes @ground (eliminationBisubs vars)

eliminateOneSidedTypeVars ::
    forall (ground :: GroundTypeKind) a.
    (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a) =>
    Endo a
eliminateOneSidedTypeVars =
    Endo $ \expr -> let
        (setFromList -> posvars, setFromList -> negvars) = mappableGetVars @ground expr
        posonlyvars :: ListSet _
        posonlyvars = difference posvars negvars
        negonlyvars :: ListSet _
        negonlyvars = difference negvars posvars
        in appEndo (eliminateVars @ground (posonlyvars, negonlyvars)) expr
