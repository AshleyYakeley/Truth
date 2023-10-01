module Language.Expression.Dolan.Subtype where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
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
class (DebugIsDolanGroundType ground) => IsDolanSubtypeGroundType ground where
    getSubtypeChain ::
           forall (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
           ground dva gta
        -> ground dvb gtb
        -> DolanM ground (SubtypeChain ground dva gta dvb gtb)
    tackOnTypeConvertError ::
           (Is PolarityType pola, Is PolarityType polb)
        => DolanType ground pola ta
        -> DolanType ground polb tb
        -> DolanM ground a
        -> DolanM ground a
    throwTypeConvertError ::
           (Is PolarityType pola, Is PolarityType polb)
        => DolanType ground pola ta
        -> DolanType ground polb tb
        -> DolanM ground a
    throwTypeNotInvertible :: Is PolarityType polarity => DolanType ground polarity t -> DolanM ground a

getSubtypeChainRenamed ::
       forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
       IsDolanSubtypeGroundType ground
    => ground dva gta
    -> ground dvb gtb
    -> DolanTypeCheckM ground (SubtypeChain ground dva gta dvb gtb)
getSubtypeChainRenamed ga gb = do
    chain <- lift $ getSubtypeChain ga gb
    unEndoM (renameType @(DolanTypeSystem ground) [] FreeName) chain
