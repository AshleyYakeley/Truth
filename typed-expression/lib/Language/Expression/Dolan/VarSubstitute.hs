module Language.Expression.Dolan.VarSubstitute where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

type GenShim :: PolyShimKind -> Constraint
type GenShim pshim = (LazyCategory (pshim Type), DolanVarianceInCategory pshim)

type SubShim :: PolyShimKind -> Type -> Polarity -> Type -> Type -> ShimKind Type
type SubShim pshim oldtype polarity newtypepos newtypeneg
     = PolyFuncShim ( PolarMap (pshim Type) polarity oldtype newtypepos
                    , PolarMap (pshim Type) (InvertPolarity polarity) oldtype newtypeneg) pshim Type

type VarSubstitution :: Type -> Polarity -> Type -> Type -> Type
data VarSubstitution oldtype polarity newtypepos newtypeneg where
    MkVarSubstitution
        :: forall oldname polarity newnamepos newnameneg.
           SymbolType oldname
        -> SymbolType newnamepos
        -> SymbolType newnameneg
        -> VarSubstitution (UVar Type oldname) polarity (UVar Type newnamepos) (UVar Type newnameneg)

instance (RenamerNamespaceT ts ~ VarNamespaceT ts, RenamerT ts ~ VarRenamerT ts) =>
             NamespaceRenamable ts (VarSubstitution oldtype polarity newtypepos newtypeneg) where
    namespaceRename (MkVarSubstitution oldvar newvarpos newvarneg) = do
        MkVarType oldvar' <- namespaceRename @ts (MkVarType @Type oldvar)
        MkVarType newvarpos' <- namespaceRename @ts (MkVarType @Type newvarpos)
        MkVarType newvarneg' <- namespaceRename @ts (MkVarType @Type newvarneg)
        return $ MkVarSubstitution oldvar' newvarpos' newvarneg'

mkPolarVarSubstitution ::
       forall polarity oldname newname.
       SymbolType oldname
    -> SymbolType newname
    -> VarSubstitution (UVar Type oldname) polarity (UVar Type newname) (UVar Type oldname)
mkPolarVarSubstitution oldvar newvar = MkVarSubstitution oldvar newvar oldvar

class VarSubstitutable (w :: Polarity -> Type -> Type) where
    varSubstitute ::
           forall (pshim :: PolyShimKind) oldtype polarity polarity' newtypepos newtypeneg t.
           (GenShim pshim, Is PolarityType polarity, Is PolarityType polarity')
        => VarSubstitution oldtype polarity' newtypepos newtypeneg
        -> w polarity t
        -> PShimWit (SubShim pshim oldtype polarity' newtypepos newtypeneg) w polarity t

varShimSubstitute ::
       forall (w :: Polarity -> Type -> Type) (pshim :: PolyShimKind) oldtype polarity polarity' newtypepos newtypeneg t.
       (GenShim pshim, VarSubstitutable w, Is PolarityType polarity, Is PolarityType polarity')
    => VarSubstitution oldtype polarity' newtypepos newtypeneg
    -> PShimWit (SubShim pshim oldtype polarity' newtypepos newtypeneg) w polarity t
    -> PShimWit (SubShim pshim oldtype polarity' newtypepos newtypeneg) w polarity t
varShimSubstitute sub = chainShimWit (varSubstitute sub)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => VarSubstitutable (DolanSingularType ground) where
    varSubstitute ::
           forall (pshim :: PolyShimKind) oldtype polarity polarity' newtypepos newtypeneg t.
           (GenShim pshim, Is PolarityType polarity, Is PolarityType polarity')
        => VarSubstitution oldtype polarity' newtypepos newtypeneg
        -> DolanSingularType ground polarity t
        -> PShimWit (SubShim pshim oldtype polarity' newtypepos newtypeneg) (DolanSingularType ground) polarity t
    varSubstitute (MkVarSubstitution svar varpos varneg) (VarDolanSingularType var)
        | Just Refl <- testEquality svar var =
            case samePolarity @polarity @polarity' of
                Left Refl -> MkShimWit (VarDolanSingularType varpos) $ mkPolarPolyFuncShim $ \(convp, _) -> convp
                Right Refl -> MkShimWit (VarDolanSingularType varneg) $ mkPolarPolyFuncShim $ \(_, convn) -> convn
    varSubstitute (MkVarSubstitution svar _ _) t@(RecursiveDolanSingularType var _)
        | Just Refl <- testEquality svar var = mkShimWit t
    varSubstitute sub t@(RecursiveDolanSingularType oldvar pt) =
        invertPolarity @polarity $ let
            -- find a name that isn't free in either sub or t,
            -- if possible the same name as oldvar
            newname =
                runIdentity $
                runVarRenamerT $ do
                    runVarNamespaceT $ do
                        _ <- dolanNamespaceRename @ground t
                        _ <- dolanNamespaceRename @ground sub
                        return ()
                    varRenamerTGenerate [uVarName oldvar]
            in newUVar newname $ \newvar ->
                   case varSubstitute @_ @pshim (mkPolarVarSubstitution @polarity oldvar newvar) pt of
                       MkShimWit pt' vconv ->
                           case varSubstitute @_ @pshim sub pt' of
                               MkShimWit pt'' sconv ->
                                   assignUVarWit newvar pt'' $
                                   MkShimWit (RecursiveDolanSingularType newvar pt'') $
                                   mkPolarPolyFuncShim $ \vars -> let
                                       conv =
                                           applyPolarPolyFuncShim sconv vars <.>
                                           applyPolarPolyFuncShim vconv (lazyPolarMap conv, cid)
                                       in conv
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
    invertPolarity @polarity $
    case varSubstitute (mkPolarVarSubstitution @polarity oldvar newvar) wt of
        MkShimWit wt' rconv -> MkShimWit wt' $ applyPolarPolyFuncShim rconv (conv, id)

recursiveMapType ::
       forall (ground :: GroundTypeKind) polarity (name :: Symbol). (IsDolanGroundType ground, Is PolarityType polarity)
    => (forall a. DolanType ground polarity a -> DolanSemiIsoShimWit ground polarity a)
    -> SymbolType name
    -> DolanType ground polarity (UVar Type name)
    -> DolanSemiIsoSingularShimWit ground polarity (UVar Type name)
recursiveMapType f oldvar pt = let
    newname =
        runIdentity $
        runVarRenamerT $ do
            runVarNamespaceT $ do
                _ <- dolanNamespaceRename @ground pt
                return ()
            varRenamerTGenerate [uVarName oldvar]
    in newUVar newname $ \newvar ->
           invertPolarity @polarity $ let
               sub :: VarSubstitution _ polarity _ _
               sub = mkPolarVarSubstitution oldvar newvar
               in case varSubstitute @_ @(DolanPolySemiIsoShim ground) sub pt of
                      MkShimWit pt' rconv ->
                          case f pt' of
                              MkShimWit tpt jconv ->
                                  assignUVarWit newvar tpt $ let
                                      conv = jconv . (applyPolarPolyFuncShim rconv (lazyPolarMap conv, id))
                                      in MkShimWit (RecursiveDolanSingularType newvar tpt) conv

recursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity name t.
       (IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity)
    => SymbolType name
    -> PolarMap (pshim Type) polarity (UVar Type name) t
    -> PShimWit (pshim Type) (DolanType ground) polarity t
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity t
recursiveDolanShimWit oldvar vconv (MkShimWit t sconv) =
    invertPolarity @polarity $
    newUVar (uVarName oldvar) $ \newvar ->
        case varSubstitute @_ @pshim (mkPolarVarSubstitution @polarity oldvar newvar) t of
            MkShimWit t' rconv ->
                assignUVarWit newvar t' $ let
                    conv = (applyPolarPolyFuncShim rconv (lazyPolarMap conv . vconv, id)) . sconv
                    in MkShimWit (RecursiveDolanSingularType newvar t') conv

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
mapDolanSingularType ff (RecursiveDolanSingularType var t) = recursiveDolanShimWit var id $ ff t

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
    return $ recursiveDolanShimWit var id t'
