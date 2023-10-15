module Language.Expression.Dolan.Subtype where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Solver.CrumbleM
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Shapes

type SubtypeLink :: GroundTypeKind -> forall (dva :: CCRVariances) ->
                                              CCRVariancesKind dva -> forall (dvb :: CCRVariances) ->
                                                                              CCRVariancesKind dvb -> Type
data SubtypeLink ground dva gta dvb gtb =
    forall a b. MkSubtypeLink (CCRVariancesMap dva gta)
                              (CCRPolarArguments dva (DolanType ground) gta 'Negative a)
                              (CCRPolarArguments dvb (DolanType ground) gtb 'Positive b)
                              (DolanOpenExpression ground (DolanShim ground a b))

instance forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb). IsDolanGroundType ground =>
             VarRenameable (SubtypeLink ground dva gta dvb gtb) where
    varRename rs =
        MkEndoM $ \(MkSubtypeLink dvma argsa argsb expr) -> do
            argsa' <- unEndoM (dolanArgumentsVarRename rs) argsa
            argsb' <- unEndoM (dolanArgumentsVarRename rs) argsb
            return $ MkSubtypeLink dvma argsa' argsb' expr

instance forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb). (IsDolanGroundType ground) =>
             Show (SubtypeLink ground dva gta dvb gtb) where
    show (MkSubtypeLink _ _ _ _) = "link"

type SubtypeChain :: GroundTypeKind -> forall (dva :: CCRVariances) ->
                                               CCRVariancesKind dva -> forall (dvb :: CCRVariances) ->
                                                                               CCRVariancesKind dvb -> Type
data SubtypeChain ground dva gta dvb gtb where
    NilSubtypeChain
        :: forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
           SubtypeChain ground dv gt dv gt
    ConsSubtypeChain
        :: forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb) (dvc :: CCRVariances) (gtc :: CCRVariancesKind dvc).
           SubtypeLink ground dvb gtb dvc gtc
        -> SubtypeChain ground dva gta dvb gtb
        -> SubtypeChain ground dva gta dvc gtc

instance forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb). (IsDolanGroundType ground) =>
             Show (SubtypeChain ground dva gta dvb gtb) where
    show NilSubtypeChain = ""
    show (ConsSubtypeChain link chain) = show link <> "; " <> show chain

getChainArguments ::
       forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb) (dvc :: CCRVariances) (gtc :: CCRVariancesKind dvc) r.
       SubtypeLink ground dvb gtb dvc gtc
    -> SubtypeChain ground dva gta dvb gtb
    -> (forall ta tc.
                CCRPolarArguments dva (DolanType ground) gta 'Negative ta -> CCRPolarArguments dvc (DolanType ground) gtc 'Positive tc -> r)
    -> r
getChainArguments (MkSubtypeLink _ argsa argsc _) NilSubtypeChain call = call argsa argsc
getChainArguments (MkSubtypeLink _ _ argsc _) (ConsSubtypeChain link chain) call =
    getChainArguments link chain $ \argsa _ -> call argsa argsc

instance forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb). IsDolanGroundType ground =>
             VarRenameable (SubtypeChain ground dva gta dvb gtb) where
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
       SubtypeChain ground dvb gtb dvc gtc
    -> SubtypeChain ground dva gta dvb gtb
    -> SubtypeChain ground dva gta dvc gtc
composeSubtypeChain NilSubtypeChain chain = chain
composeSubtypeChain (ConsSubtypeChain link chaina) chainb = ConsSubtypeChain link $ composeSubtypeChain chaina chainb

linkSubtypeChain ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb ta tb.
       (CCRVariancesMap dva gta)
    -> (CCRPolarArguments dva (DolanType ground) gta 'Negative ta)
    -> (CCRPolarArguments dvb (DolanType ground) gtb 'Positive tb)
    -> (DolanOpenExpression ground (DolanShim ground ta tb))
    -> SubtypeChain ground dva gta dvb gtb
linkSubtypeChain vmap argsa argsb expr = ConsSubtypeChain (MkSubtypeLink vmap argsa argsb expr) NilSubtypeChain

type IsDolanSubtypeGroundType :: GroundTypeKind -> Constraint
class IsDolanGroundType ground => IsDolanSubtypeGroundType ground where
    getSubtypeChain ::
           forall (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
           ground dva gta
        -> ground dvb gtb
        -> DolanM ground (TypeResult ground (SubtypeChain ground dva gta dvb gtb))
    throwTypeError :: TypeError ground -> DolanM ground a

runTypeResult ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => TypeResult ground a
    -> DolanM ground a
runTypeResult (SuccessResult a) = return a
runTypeResult (FailureResult err) = throwTypeError err

runCrumbleMRigidity ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => (String -> NameRigidity)
    -> CrumbleM ground a
    -> DolanTypeCheckM ground a
runCrumbleMRigidity rigidity cra = do
    rea <- runCrumbleMResult rigidity cra
    lift $ runTypeResult rea

runCrumbleM ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => CrumbleM ground a
    -> DolanTypeCheckM ground a
runCrumbleM cra = do
    rigidity <- renamerGetNameRigidity
    runCrumbleMRigidity rigidity cra

getSubtypeChainRenamed ::
       forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
       IsDolanSubtypeGroundType ground
    => ground dva gta
    -> ground dvb gtb
    -> CrumbleM ground (SubtypeChain ground dva gta dvb gtb)
getSubtypeChainRenamed ga gb =
    liftFullToCrumbleMWithUnlift $ \_ -> do
        rchain <- lift $ getSubtypeChain ga gb
        for rchain $ unEndoM (renameType @(DolanTypeSystem ground) [] FreeName)
