{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.SubtypeChain where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

type SubtypeLink ::
    GroundTypeKind ->
    forall (dva :: CCRVariances) ->
    CCRVariancesKind dva -> forall (dvb :: CCRVariances) -> CCRVariancesKind dvb -> Type
data SubtypeLink ground dva gta dvb gtb
    = forall a b. MkSubtypeLink
        (CCRVariancesMap dva gta)
        (CCRPolarArguments dva (DolanType ground) gta 'Negative a)
        (CCRPolarArguments dvb (DolanType ground) gtb 'Positive b)
        (DolanOpenExpression ground (DolanShim ground a b))

instance
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    IsDolanGroundType ground =>
    VarRenameable (SubtypeLink ground dva gta dvb gtb)
    where
    varRename rs =
        MkEndoM $ \(MkSubtypeLink dvma argsa argsb expr) -> do
            argsa' <- unEndoM (dolanArgumentsVarRename rs) argsa
            argsb' <- unEndoM (dolanArgumentsVarRename rs) argsb
            return $ MkSubtypeLink dvma argsa' argsb' expr

instance
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    IsDolanGroundType ground =>
    Show (SubtypeLink ground dva gta dvb gtb)
    where
    show (MkSubtypeLink _ _ _ _) = "link"

type SubtypeChain ::
    GroundTypeKind ->
    forall (dva :: CCRVariances) ->
    CCRVariancesKind dva -> forall (dvb :: CCRVariances) -> CCRVariancesKind dvb -> Type
data SubtypeChain ground dva gta dvb gtb where
    NilSubtypeChain ::
        forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
        SubtypeChain ground dv gt dv gt
    ConsSubtypeChain ::
        forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb) (dvc :: CCRVariances) (gtc :: CCRVariancesKind dvc).
        SubtypeLink ground dvb gtb dvc gtc ->
        SubtypeChain ground dva gta dvb gtb ->
        SubtypeChain ground dva gta dvc gtc

instance
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    IsDolanGroundType ground =>
    Show (SubtypeChain ground dva gta dvb gtb)
    where
    show NilSubtypeChain = ""
    show (ConsSubtypeChain link chain) = show link <> "; " <> show chain

getChainArguments ::
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb) (dvc :: CCRVariances) (gtc :: CCRVariancesKind dvc) r.
    SubtypeLink ground dvb gtb dvc gtc ->
    SubtypeChain ground dva gta dvb gtb ->
    ( forall ta tc.
      CCRPolarArguments dva (DolanType ground) gta 'Negative ta -> CCRPolarArguments dvc (DolanType ground) gtc 'Positive tc -> r
    ) ->
    r
getChainArguments (MkSubtypeLink _ argsa argsc _) NilSubtypeChain call = call argsa argsc
getChainArguments (MkSubtypeLink _ _ argsc _) (ConsSubtypeChain link chain) call =
    getChainArguments link chain $ \argsa _ -> call argsa argsc

instance
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    IsDolanGroundType ground =>
    VarRenameable (SubtypeChain ground dva gta dvb gtb)
    where
    varRename rs =
        MkEndoM $ \case
            NilSubtypeChain -> pure NilSubtypeChain
            ConsSubtypeChain link chain ->
                liftA2 ConsSubtypeChain (unEndoM (varRename rs) link) (unEndoM (varRename rs) chain)

identitySubtypeChain ::
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
    SubtypeChain ground dv gt dv gt
identitySubtypeChain = NilSubtypeChain

composeSubtypeChain ::
    forall (ground :: GroundTypeKind) dva gta dvb gtb dvc gtc.
    SubtypeChain ground dvb gtb dvc gtc ->
    SubtypeChain ground dva gta dvb gtb ->
    SubtypeChain ground dva gta dvc gtc
composeSubtypeChain NilSubtypeChain chain = chain
composeSubtypeChain (ConsSubtypeChain link chaina) chainb = ConsSubtypeChain link $ composeSubtypeChain chaina chainb

linkSubtypeChain ::
    forall (ground :: GroundTypeKind) dva gta dvb gtb ta tb.
    (CCRVariancesMap dva gta) ->
    (CCRPolarArguments dva (DolanType ground) gta 'Negative ta) ->
    (CCRPolarArguments dvb (DolanType ground) gtb 'Positive tb) ->
    (DolanOpenExpression ground (DolanShim ground ta tb)) ->
    SubtypeChain ground dva gta dvb gtb
linkSubtypeChain vmap argsa argsb expr = ConsSubtypeChain (MkSubtypeLink vmap argsa argsb expr) NilSubtypeChain

newtype GetSubtypeChain (ground :: GroundTypeKind) = MkGetSubtypeChain
    { unGetSubtypeChain ::
        forall (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
        ground dva gta ->
        ground dvb gtb ->
        TypeResult ground (SubtypeChain ground dva gta dvb gtb)
    }

type DolanTypeM :: GroundTypeKind -> Type -> Type
type DolanTypeM ground = ReaderT (GetSubtypeChain ground) (TypeResult ground)

runDolanTypeM :: forall (ground :: GroundTypeKind). GetSubtypeChain ground -> DolanTypeM ground --> TypeResult ground
runDolanTypeM gsc ma = runReaderT ma gsc

type DolanRenameTypeM :: GroundTypeKind -> Type -> Type
type DolanRenameTypeM ground = DolanRenamerT ground (DolanTypeM ground)

joinTypeCheckResult :: forall (ground :: GroundTypeKind) a. DolanRenameTypeM ground a -> DolanRenameTypeM ground a -> DolanRenameTypeM ground a
joinTypeCheckResult tma tmb = tunnel $ \tun1 -> tunnel $ \tun2 -> joinFirstResult (tun2 $ tun1 tma) (tun2 $ tun1 tmb)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TypeSystem (DolanTypeSystem ground) where
    type TSOuter (DolanTypeSystem ground) = DolanRenameTypeM ground
    type TSNegWitness (DolanTypeSystem ground) = DolanType ground 'Negative
    type TSPosWitness (DolanTypeSystem ground) = DolanType ground 'Positive
    type TSShim (DolanTypeSystem ground) = DolanShim ground
    type TSVarID (DolanTypeSystem ground) = DolanVarID ground

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => RenameTypeSystem (DolanTypeSystem ground) where
    type RenamerT (DolanTypeSystem ground) = DolanRenamerT ground
    type RenamerNamespaceT (DolanTypeSystem ground) = VarNamespaceT (DolanTypeSystem ground)
    namespaceRenameSource = varNamespaceRenameSource
    renameNewFreeVar = do
        n <- renamerGenerateFree
        newTypeVar n $ \v -> return $ MkNewVar (varDolanShimWit v) (varDolanShimWit v)
    namespace fn rgd = runVarNamespaceT fn rgd
    runRenamer = runVarRenamerT
    finalRenamer = finalVarRenamerT

dolanNamespaceRenameArguments ::
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv) polarity m.
    (IsDolanGroundType ground, Monad m, Is PolarityType polarity) =>
    EndoM' (VarNamespaceT (DolanTypeSystem ground) (DolanRenamerT ground m)) (CCRPolarArguments dv (DolanType ground) gt polarity)
dolanNamespaceRenameArguments = pureMapDolanArgumentsM $ namespaceRenameType @(DolanTypeSystem ground)

type DebugGroundType :: GroundTypeKind -> Constraint
class
    ( Show (Exc (DolanTypeM ground))
    , MonadIO (DolanTypeM ground)
    , DolanPolyShim ground Type ~ JMShim Type
    , ShowGroundType ground
    ) =>
    DebugGroundType ground

instance
    forall (ground :: GroundTypeKind).
    ( Show (Exc (DolanTypeM ground))
    , MonadIO (DolanTypeM ground)
    , DolanPolyShim ground Type ~ JMShim Type
    , ShowGroundType ground
    ) =>
    DebugGroundType ground
