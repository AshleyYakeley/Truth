module Language.Expression.Dolan.Solver
    ( DolanTypeCheckM
    , liftTypeCheck
    , Solver
    , solverLiftExpression
    , solverLiftM
    , solverOpenExpression
    , runSolver
    , solveRecursiveTypes
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unroll
import Shapes

liftTypeCheck ::
       forall (ground :: GroundTypeKind) a. Monad (DolanM ground)
    => DolanM ground a
    -> DolanTypeCheckM ground a
liftTypeCheck ma = lift ma

type ShimType :: GroundTypeKind -> Type -> Type
data ShimType ground t where
    MkShimType
        :: forall (ground :: GroundTypeKind) pola polb a b.
           PolarityType pola
        -> PolarityType polb
        -> DolanType ground pola a
        -> DolanType ground polb b
        -> ShimType ground (DolanPolyShim ground Type a b)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (ShimType ground) where
    testEquality (MkShimType pa1 pb1 ta1 tb1) (MkShimType pa2 pb2 ta2 tb2) = do
        Refl <- testEquality pa1 pa2
        Refl <- testEquality pb1 pb2
        Refl <- testEquality ta1 ta2
        Refl <- testEquality tb1 tb2
        return Refl

type Solver :: GroundTypeKind -> (Type -> Type) -> Type -> Type
newtype Solver ground wit a = MkSolver
    { unSolver :: forall (rlist :: [Type]).
                          ReaderT (ListType (ShimType ground) rlist) (DolanTypeCheckM ground) (Expression wit (HList rlist -> a))
    }

instance forall (ground :: GroundTypeKind) wit. Functor (DolanM ground) => Functor (Solver ground wit) where
    fmap ab (MkSolver ruha) = MkSolver $ (fmap $ fmap $ fmap ab) ruha

instance forall (ground :: GroundTypeKind) wit. Monad (DolanM ground) => Applicative (Solver ground wit) where
    pure a = MkSolver $ pure $ pure $ pure a
    MkSolver ruhab <*> MkSolver ruha =
        MkSolver $ (\uhab uha -> (\hab ha h -> hab h $ ha h) <$> uhab <*> uha) <$> ruhab <*> ruha

instance forall (ground :: GroundTypeKind) wit. MonadPlus (DolanM ground) => Alternative (Solver ground wit) where
    empty = MkSolver empty
    MkSolver p <|> MkSolver q = MkSolver $ p <|> q

solverLiftExpression ::
       forall (ground :: GroundTypeKind) wit a. IsDolanSubtypeGroundType ground
    => Expression wit a
    -> Solver ground wit a
solverLiftExpression ua = MkSolver $ pure $ fmap pure ua

solverLiftM ::
       forall (ground :: GroundTypeKind) wit a. IsDolanSubtypeGroundType ground
    => DolanM ground a
    -> Solver ground wit a
solverLiftM tca = MkSolver $ lift $ fmap (pure . pure) $ liftTypeCheck tca

solverMapExpression ::
       forall (ground :: GroundTypeKind) wit a b. IsDolanSubtypeGroundType ground
    => (forall x. Expression wit (x -> a) -> Expression wit (x -> b))
    -> Solver ground wit a
    -> Solver ground wit b
solverMapExpression ff (MkSolver ma) = MkSolver $ fmap ff ma

solverOpenExpression ::
       forall (ground :: GroundTypeKind) wit t a. IsDolanSubtypeGroundType ground
    => wit t
    -> Solver ground wit (t -> a)
    -> Solver ground wit a
solverOpenExpression wit = solverMapExpression $ \expr -> OpenExpression wit $ fmap (\xta t x -> xta x t) expr

runSolver ::
       forall (ground :: GroundTypeKind) wit a. IsDolanSubtypeGroundType ground
    => Solver ground wit a
    -> DolanTypeCheckM ground (Expression wit a)
runSolver (MkSolver rma) = fmap (fmap $ \ua -> ua ()) $ runReaderT rma NilListType

solveRecursiveTypes ::
       forall (ground :: GroundTypeKind) pola polb wit a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => (forall pa pb.
                DolanPlainType ground pola pa -> DolanPlainType ground polb pb -> Solver ground wit (DolanPolyShim ground Type pa pb))
    -> DolanType ground pola a
    -> DolanType ground polb b
    -> Solver ground wit (DolanPolyShim ground Type a b)
solveRecursiveTypes solvePlainTypes (PlainDolanType pta) (PlainDolanType ptb) = solvePlainTypes pta ptb
solveRecursiveTypes solvePlainTypes ta tb =
    invertPolarity @polb $
    MkSolver $ do
        let st = MkShimType (polarityType @pola) (polarityType @polb) ta tb
        rcs <- ask
        case lookUpListElement st rcs of
            Just lelem -> return $ pure $ getListElement lelem
            Nothing ->
                withReaderT (\rcs' -> ConsListType st rcs') $ do
                    MkShimWit pta iconva <- return $ dolanTypeToPlainUnroll ta
                    conva <- mpure $ polarPolySemiIsoPositive iconva
                    MkShimWit ptb iconvb <- return $ dolanTypeToPlainUnroll tb
                    convb <- mpure $ polarPolySemiIsoNegative iconvb
                    erconv <- unSolver $ solvePlainTypes pta ptb
                    let
                        fixconv rconv rl = let
                            conv = convb <.> rconv (lazyEnhanced conv, rl) <.> conva
                            in conv
                    return $ fmap fixconv erconv
