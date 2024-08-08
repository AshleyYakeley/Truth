module Language.Expression.Dolan.Bisubstitute.Bisubstitution where

import Data.Shim
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem
import Shapes

type Bisubstitution :: GroundTypeKind -> ShimKind Type -> (Type -> Type) -> Type
data Bisubstitution ground shim m =
    forall tv. MkBisubstitution (TypeVarT tv)
                                (m (PShimWit shim (DolanType ground) 'Positive tv))
                                (m (PShimWit shim (DolanType ground) 'Negative tv))

instance forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m. ( MonadInner m
         , AllConstraint Show (DolanType ground 'Positive)
         , AllConstraint Show (DolanType ground 'Negative)
         ) => Show (Bisubstitution ground shim m) where
    show (MkBisubstitution var mtpos mtneg) = let
        svar = show var
        spos =
            case mToMaybe mtpos of
                Just (MkShimWit t _) -> allShow t
                Nothing -> "FAILS"
        sneg =
            case mToMaybe mtneg of
                Just (MkShimWit t _) -> allShow t
                Nothing -> "FAILS"
        in "{" <> svar <> "+ => " <> spos <> "; " <> svar <> "- => " <> sneg <> "}"

instance forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m. (IsDolanGroundType ground, Traversable m) =>
             VarRenameable (Bisubstitution ground shim m) where
    varRename ev =
        MkEndoM $ \(MkBisubstitution var mpos mneg) -> do
            mpos' <- unEndoM (endoFor $ varRename ev) mpos
            mneg' <- unEndoM (endoFor $ varRename ev) mneg
            return $ MkBisubstitution var mpos' mneg'
