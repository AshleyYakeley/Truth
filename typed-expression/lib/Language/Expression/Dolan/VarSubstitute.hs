module Language.Expression.Dolan.VarSubstitute where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Recursive
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

type GenShim :: PolyShimKind -> Constraint
type GenShim pshim = (LazyCategory (pshim Type), DolanVarianceInCategory pshim)

type VarSubstitution :: PolyShimKind -> Type
data VarSubstitution pshim where
    MkVarSubstitution
        :: forall (pshim :: PolyShimKind) oldname.
           SymbolType oldname
        -> ShimWit (pshim Type) VarType 'Positive (UVar Type oldname)
        -> ShimWit (pshim Type) VarType 'Negative (UVar Type oldname)
        -> VarSubstitution pshim

instance forall ts (pshim :: PolyShimKind). ( RenameTypeSystem ts
         , RenamerNamespaceT ts ~ VarNamespaceT ts
         , RenamerT ts ~ VarRenamerT ts
         ) => NamespaceRenamable ts (VarSubstitution pshim) where
    namespaceRename (MkVarSubstitution oldvar newpos newneg) = do
        MkVarType oldvar' <- namespaceRename @ts (MkVarType @Type oldvar)
        newpos' <- namespaceRename @ts newpos
        newneg' <- namespaceRename @ts newneg
        return $ MkVarSubstitution oldvar' newpos' newneg'

mkPolarVarSubstitution ::
       forall (pshim :: PolyShimKind) polarity oldname. (GenShim pshim, Is PolarityType polarity)
    => SymbolType oldname
    -> ShimWit (pshim Type) VarType polarity (UVar Type oldname)
    -> VarSubstitution pshim
mkPolarVarSubstitution oldvar newwit =
    case polarityType @polarity of
        PositiveType -> MkVarSubstitution oldvar newwit (mkShimWit $ MkVarType oldvar)
        NegativeType -> MkVarSubstitution oldvar (mkShimWit $ MkVarType oldvar) newwit

class VarSubstitutable (w :: Polarity -> Type -> Type) where
    varSubstitute ::
           forall (pshim :: PolyShimKind) polarity t. (GenShim pshim, Is PolarityType polarity)
        => VarSubstitution pshim
        -> w polarity t
        -> PShimWit (pshim Type) w polarity t

varShimSubstitute ::
       forall (w :: Polarity -> Type -> Type) (pshim :: PolyShimKind) polarity t.
       (GenShim pshim, VarSubstitutable w, Is PolarityType polarity)
    => VarSubstitution pshim
    -> PShimWit (pshim Type) w polarity t
    -> PShimWit (pshim Type) w polarity t
varShimSubstitute sub = chainShimWit (varSubstitute sub)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => VarSubstitutable (DolanSingularType ground) where
    varSubstitute ::
           forall (pshim :: PolyShimKind) polarity t. (GenShim pshim, Is PolarityType polarity)
        => VarSubstitution pshim
        -> DolanSingularType ground polarity t
        -> PShimWit (pshim Type) (DolanSingularType ground) polarity t
    varSubstitute (MkVarSubstitution oldvar newpos newneg) (VarDolanSingularType var)
        | Just Refl <- testEquality oldvar var =
            case polarityType @polarity of
                PositiveType -> chainShimWit (\(MkVarType v) -> mkShimWit $ VarDolanSingularType v) newpos
                NegativeType -> chainShimWit (\(MkVarType v) -> mkShimWit $ VarDolanSingularType v) newneg
    varSubstitute (MkVarSubstitution svar _ _) t@(RecursiveDolanSingularType var _)
        | Just Refl <- testEquality svar var = mkShimWit t
    varSubstitute sub t@(RecursiveDolanSingularType oldvar pt) =
        invertPolarity @polarity $
        runIdentity $
        runVarRenamerT $ do
            runVarNamespaceT $ do
                        -- find a name that isn't free in either sub or t,
                        -- if possible the same name as oldvar
                _ <- dolanNamespaceRename @ground t
                _ <- dolanNamespaceRename @ground sub
                MkVarType newvar :: VarType ntype <- varNamespaceTRenameUVar @Type oldvar
                pt' <- dolanNamespaceRename @ground pt
                MkShimWit pt'' conv <- return $ varSubstitute @_ @pshim sub pt'
                assignUVar @Type @ntype oldvar $
                    return $ MkShimWit (RecursiveDolanSingularType newvar pt'') $ shimMapRecursive conv
    varSubstitute sub t = mapDolanSingularType (varSubstitute @_ @pshim sub) t

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => VarSubstitutable (DolanType ground) where
    varSubstitute _sub NilDolanType = nilDolanShimWit
    varSubstitute sub (ConsDolanType t1 tr) = consDolanShimWit (varSubstitute sub t1) (varSubstitute sub tr)

polarVarSubstitute ::
       forall (pshim :: PolyShimKind) polarity oldname newname w t.
       (GenShim pshim, Is PolarityType polarity, VarSubstitutable w)
    => SymbolType oldname
    -> SymbolType newname
    -> PolarMap (pshim Type) polarity (UVar Type oldname) (UVar Type newname)
    -> w polarity t
    -> PShimWit (pshim Type) w polarity t
polarVarSubstitute oldvar newvar conv wt =
    varSubstitute (mkPolarVarSubstitution @pshim @polarity oldvar $ MkShimWit (MkVarType newvar) conv) wt

recursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity name t.
       (IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity)
    => SymbolType name
    -> PShimWit (pshim Type) (DolanType ground) polarity t
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity (Recursive (UVar Type name) t)
recursiveDolanShimWit var (MkShimWit t conv) = MkShimWit (RecursiveDolanSingularType var t) $ shimMapRecursive conv

mapDolanGroundArguments ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity dv gt t.
       (IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> PShimWit (pshim Type) (DolanType ground) polarity' t')
    -> ground dv gt
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity t
mapDolanGroundArguments ff g args =
    case mapDolanArguments ff (groundTypeVarianceType g) (groundTypeVarianceMap g) args of
        MkShimWit args' conv -> MkShimWit (GroundDolanSingularType g args') conv

mapDolanSingularType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> PShimWit (pshim Type) (DolanType ground) polarity' t')
    -> DolanSingularType ground polarity t
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity t
mapDolanSingularType ff (GroundDolanSingularType gt args) = mapDolanGroundArguments ff gt args
mapDolanSingularType _ t@(VarDolanSingularType _) = mkShimWit t
mapDolanSingularType ff (RecursiveDolanSingularType var t) = recursiveDolanShimWit var $ ff t

mapDolanGroundArgumentsM ::
       forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity dv gt t.
       (Monad m, IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> m (PShimWit (pshim Type) (DolanType ground) polarity' t'))
    -> ground dv gt
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> m (PShimWit (pshim Type) (DolanSingularType ground) polarity t)
mapDolanGroundArgumentsM ff g args = do
    MkShimWit args' conv <- mapDolanArgumentsM ff (groundTypeVarianceType g) (groundTypeVarianceMap g) args
    return $ MkShimWit (GroundDolanSingularType g args') conv

mapDolanSingularTypeM ::
       forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (Monad m, IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> m (PShimWit (pshim Type) (DolanType ground) polarity' t'))
    -> DolanSingularType ground polarity t
    -> m (PShimWit (pshim Type) (DolanSingularType ground) polarity t)
mapDolanSingularTypeM ff (GroundDolanSingularType gt args) = mapDolanGroundArgumentsM ff gt args
mapDolanSingularTypeM _ t@(VarDolanSingularType _) = return $ mkShimWit t
mapDolanSingularTypeM ff (RecursiveDolanSingularType var t) = do
    t' <- ff t
    return $ recursiveDolanShimWit var t'
