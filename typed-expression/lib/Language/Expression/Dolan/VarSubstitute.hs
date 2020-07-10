module Language.Expression.Dolan.VarSubstitute where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type SubShim :: GroundTypeKind -> Type -> Polarity -> Type -> Type -> ShimKind Type
type SubShim ground oldtype polarity newtypepos newtypeneg
     = PolyFuncShim ( PolarMap (DolanPolySemiIsoShim ground Type) polarity oldtype newtypepos
                    , PolarMap (DolanPolySemiIsoShim ground Type) (InvertPolarity polarity) oldtype newtypeneg) (DolanPolySemiIsoShim ground) Type

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

class VarSubstitutable (ground :: GroundTypeKind) (w :: Polarity -> Type -> Type) | w -> ground where
    varSubstitute ::
           forall oldtype polarity polarity' newtypepos newtypeneg t.
           (Is PolarityType polarity, Is PolarityType polarity')
        => VarSubstitution oldtype polarity' newtypepos newtypeneg
        -> w polarity t
        -> PShimWit (SubShim ground oldtype polarity' newtypepos newtypeneg) w polarity t

varShimSubstitute ::
       forall (ground :: GroundTypeKind) (w :: Polarity -> Type -> Type) oldtype polarity polarity' newtypepos newtypeneg t.
       (IsDolanGroundType ground, VarSubstitutable ground w, Is PolarityType polarity, Is PolarityType polarity')
    => VarSubstitution oldtype polarity' newtypepos newtypeneg
    -> PShimWit (SubShim ground oldtype polarity' newtypepos newtypeneg) w polarity t
    -> PShimWit (SubShim ground oldtype polarity' newtypepos newtypeneg) w polarity t
varShimSubstitute sub = chainShimWit (varSubstitute sub)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground =>
             VarSubstitutable ground (DolanSingularType ground) where
    varSubstitute ::
           forall oldtype polarity polarity' newtypepos newtypeneg t.
           (Is PolarityType polarity, Is PolarityType polarity')
        => VarSubstitution oldtype polarity' newtypepos newtypeneg
        -> DolanSingularType ground polarity t
        -> PShimWit (SubShim ground oldtype polarity' newtypepos newtypeneg) (DolanSingularType ground) polarity t
    varSubstitute (MkVarSubstitution svar varpos varneg) (VarDolanSingularType var)
        | Just Refl <- testEquality svar var =
            case samePolarity @polarity @polarity' of
                Left Refl -> MkShimWit (VarDolanSingularType varpos) $ mkPolarPolyFuncShim $ \(convp, _) -> convp
                Right Refl -> MkShimWit (VarDolanSingularType varneg) $ mkPolarPolyFuncShim $ \(_, convn) -> convn
    varSubstitute sub t = mapDolanSingularType (varSubstitute sub) t

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => VarSubstitutable ground (DolanPlainType ground) where
    varSubstitute _sub NilDolanPlainType = nilDolanPlainShimWit
    varSubstitute sub (ConsDolanPlainType t1 tr) = consDolanPlainShimWit (varSubstitute sub t1) (varSubstitute sub tr)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => VarSubstitutable ground (DolanType ground) where
    varSubstitute ::
           forall oldtype polarity polarity' newtypepos newtypeneg t.
           (Is PolarityType polarity, Is PolarityType polarity')
        => VarSubstitution oldtype polarity' newtypepos newtypeneg
        -> DolanType ground polarity t
        -> PShimWit (SubShim ground oldtype polarity' newtypepos newtypeneg) (DolanType ground) polarity t
    varSubstitute sub (PlainDolanType pt) = chainShimWit (\pt' -> mkShimWit $ PlainDolanType pt') $ varSubstitute sub pt
    varSubstitute (MkVarSubstitution svar _ _) t@(RecursiveDolanType var _)
        | Just Refl <- testEquality svar var = mkShimWit t
    varSubstitute sub t@(RecursiveDolanType oldvar pt) =
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
                   case varSubstitute (mkPolarVarSubstitution @polarity oldvar newvar) pt of
                       MkShimWit pt' vconv ->
                           case varSubstitute sub pt' of
                               MkShimWit pt'' sconv ->
                                   assignUVarWit newvar pt'' $
                                   MkShimWit (RecursiveDolanType newvar pt'') $
                                   mkPolarPolyFuncShim $ \vars -> let
                                       conv =
                                           applyPolarPolyFuncShim sconv vars <.>
                                           applyPolarPolyFuncShim vconv (lazyPolarSemiIso conv, cid)
                                       in conv

polarVarSubstitute ::
       forall (ground :: GroundTypeKind) polarity oldname newname w t.
       (IsDolanGroundType ground, Is PolarityType polarity, VarSubstitutable ground w)
    => SymbolType oldname
    -> SymbolType newname
    -> PolarMap (DolanPolySemiIsoShim ground Type) polarity (UVar Type oldname) (UVar Type newname)
    -> w polarity t
    -> PShimWit (DolanPolySemiIsoShim ground Type) w polarity t
polarVarSubstitute oldvar newvar conv wt =
    invertPolarity @polarity $
    case varSubstitute (mkPolarVarSubstitution @polarity oldvar newvar) wt of
        MkShimWit wt' rconv -> MkShimWit wt' $ applyPolarPolyFuncShim rconv (conv, id)
