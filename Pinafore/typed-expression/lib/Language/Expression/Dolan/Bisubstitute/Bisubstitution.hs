module Language.Expression.Dolan.Bisubstitute.Bisubstitution where

import Data.Shim
import Shapes

import Language.Expression.TypeSystem

type Bisubstitution :: (Polarity -> Type -> Type) -> ShimKind Type -> (Type -> Type) -> Type
data Bisubstitution w shim m
    = forall tv. MkBisubstitution
        (TypeVarT tv)
        (m (PShimWit shim w 'Positive tv))
        (m (PShimWit shim w 'Negative tv))

instance
    forall (w :: Polarity -> Type -> Type) (shim :: ShimKind Type) m.
    ( MonadInner m
    , AllConstraint Show (w 'Positive)
    , AllConstraint Show (w 'Negative)
    ) =>
    Show (Bisubstitution w shim m)
    where
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

instance
    forall (w :: Polarity -> Type -> Type) (shim :: ShimKind Type) m.
    (forall t. VarRenameable (w 'Positive t), forall t. VarRenameable (w 'Negative t), Traversable m) =>
    VarRenameable (Bisubstitution w shim m)
    where
    varRename ev =
        MkEndoM $ \(MkBisubstitution var mpos mneg) -> do
            mpos' <- unEndoM (endoFor $ varRename ev) mpos
            mneg' <- unEndoM (endoFor $ varRename ev) mneg
            return $ MkBisubstitution var mpos' mneg'
