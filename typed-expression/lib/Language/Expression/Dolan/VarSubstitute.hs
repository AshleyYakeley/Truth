module Language.Expression.Dolan.VarSubstitute
    ( VarSubstitutable(..)
    , mkPolarVarSubstitution
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.MapType
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Recursive
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

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
