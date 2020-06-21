module Language.Expression.Dolan.Solver where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type DolanTypeCheckM :: GroundTypeKind -> Type -> Type
type DolanTypeCheckM ground = VarRenamerT (DolanTypeSystem ground) (DolanM ground)

liftTypeCheck ::
       forall (ground :: GroundTypeKind) a. Monad (DolanM ground)
    => DolanM ground a
    -> DolanTypeCheckM ground a
liftTypeCheck ma = lift ma

type ShimType :: GroundTypeKind -> Polarity -> Polarity -> Type -> Type
data ShimType ground pola polb t where
    MkShimType
        :: forall (ground :: GroundTypeKind) pola polb a b.
           DolanType ground pola a
        -> DolanType ground polb b
        -> ShimType ground pola polb (DolanPolyShim ground Type a b)

instance forall (ground :: GroundTypeKind) pola polb. IsDolanGroundType ground =>
             TestEquality (ShimType ground pola polb) where
    testEquality (MkShimType ta1 tb1) (MkShimType ta2 tb2) = do
        Refl <- testEquality ta1 ta2
        Refl <- testEquality tb1 tb2
        return Refl

type Solver :: GroundTypeKind -> Polarity -> Polarity -> (Type -> Type) -> Type -> Type
newtype Solver ground pola polb wit a = MkSolver
    { unSolver :: forall (rlist :: [Type]).
                          ReaderT (ListType (ShimType ground pola polb) rlist) (DolanTypeCheckM ground) (Expression wit (HList rlist -> a))
    }

instance forall (ground :: GroundTypeKind) pola polb wit. Functor (DolanM ground) =>
             Functor (Solver ground pola polb wit) where
    fmap ab (MkSolver ruha) = MkSolver $ (fmap $ fmap $ fmap ab) ruha

instance forall (ground :: GroundTypeKind) pola polb wit. Monad (DolanM ground) =>
             Applicative (Solver ground pola polb wit) where
    pure a = MkSolver $ pure $ pure $ pure a
    MkSolver ruhab <*> MkSolver ruha =
        MkSolver $ (\uhab uha -> (\hab ha h -> hab h $ ha h) <$> uhab <*> uha) <$> ruhab <*> ruha

solverLift ::
       forall (ground :: GroundTypeKind) pola polb wit a. IsDolanSubtypeGroundType ground
    => Expression wit a
    -> Solver ground pola polb wit a
solverLift ua = MkSolver $ pure $ fmap pure ua

solverLiftTypeCheck ::
       forall (ground :: GroundTypeKind) pola polb wit a. IsDolanSubtypeGroundType ground
    => DolanTypeCheckM ground a
    -> Solver ground pola polb wit a
solverLiftTypeCheck tca = MkSolver $ lift $ fmap (pure . pure) tca

solverLiftSourceScoped ::
       forall (ground :: GroundTypeKind) pola polb wit a. IsDolanSubtypeGroundType ground
    => DolanM ground a
    -> Solver ground pola polb wit a
solverLiftSourceScoped tca = solverLiftTypeCheck $ liftTypeCheck tca

runSolver ::
       forall (ground :: GroundTypeKind) pola polb wit a. IsDolanSubtypeGroundType ground
    => Solver ground pola polb wit a
    -> DolanTypeCheckM ground (Expression wit a)
runSolver (MkSolver rma) = fmap (fmap $ \ua -> ua ()) $ runReaderT rma NilListType

solveRecursiveTypes ::
       forall (ground :: GroundTypeKind) pola polb wit a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => (forall pa pb.
                DolanPlainType ground pola pa -> DolanPlainType ground polb pb -> Solver ground pola polb wit (DolanPolyShim ground Type pa pb))
    -> DolanType ground pola a
    -> DolanType ground polb b
    -> Solver ground pola polb wit (DolanPolyShim ground Type a b)
solveRecursiveTypes solvePlainTypes (PlainDolanType pta) (PlainDolanType ptb) = solvePlainTypes pta ptb
solveRecursiveTypes solvePlainTypes ta tb =
    invertPolarity @polb $
    MkSolver $ do
        let st = MkShimType ta tb
        rcs <- ask
        case lookUpListElement st rcs of
            Just lelem -> return $ pure $ getListElement lelem
            Nothing ->
                withReaderT (ConsListType st) $ do
                    MkShimWit pta iconva <- return $ dolanTypeToPlainUnroll ta
                    MkShimWit ptb iconvb <- return $ dolanTypeToPlainUnroll tb
                    erconv <- unSolver $ solvePlainTypes pta ptb
                    let
                        fixconv rconv rl = let
                            conv =
                                polarPolyIsoSingle (invertPolarMap iconvb) <.> rconv (conv, rl) <.>
                                polarPolyIsoSingle iconva
                            in conv
                    return $ fmap fixconv erconv
