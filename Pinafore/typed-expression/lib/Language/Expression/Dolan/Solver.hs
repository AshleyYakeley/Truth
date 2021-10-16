module Language.Expression.Dolan.Solver
    ( Solver
    , solverLiftExpression
    , solverOpenExpression
    , runSolver
    , solveRecursiveTypes
    , solveRecursiveShimWits
    , solveRecursiveSingularTypes
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unroll
import Shapes

type ShimType :: GroundTypeKind -> Type -> Type
data ShimType ground t where
    MkShimType
        :: forall (ground :: GroundTypeKind) pola polb a b.
           PolarityType pola
        -> PolarityType polb
        -> RecursiveOrPlainType ground pola a
        -> RecursiveOrPlainType ground polb b
        -> ShimType ground (DolanShim ground a b)

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

instance forall (ground :: GroundTypeKind) wit. Monad (DolanM ground) => WrappedApplicative (Solver ground wit) where
    type WAInnerM (Solver ground wit) = DolanTypeCheckM ground
    wexec msa =
        MkSolver $ do
            MkSolver sa <- lift $ msa
            sa
    whoist mm (MkSolver sb) = MkSolver $ hoist mm sb

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
    => (forall pa pb. DolanType ground pola pa -> DolanType ground polb pb -> Solver ground wit (DolanShim ground pa pb))
    -> RecursiveOrPlainType ground pola a
    -> RecursiveOrPlainType ground polb b
    -> Solver ground wit (DolanShim ground a b)
solveRecursiveTypes solvePlainTypes rpta rptb =
    invertPolarity @polb $
    MkSolver $ do
        let st = MkShimType (polarityType @pola) (polarityType @polb) rpta rptb
        rcs <- ask
        case lookUpListElement st rcs of
            Just lelem -> return $ pure $ getListElement lelem
            Nothing ->
                withReaderT (\rcs' -> ConsListType st rcs') $ do
                    MkShimWit pta iconva <- return $ unrollRecursiveOrPlainType rpta
                    conva <- return $ polarPolyIsoPositive iconva
                    MkShimWit ptb iconvb <- return $ unrollRecursiveOrPlainType rptb
                    convb <- return $ polarPolyIsoNegative iconvb
                    erconv <- unSolver $ solvePlainTypes pta ptb
                    let
                        fixconv rconv rl = let
                            conv = convb <.> rconv (lazyFunctionShim conv, rl) <.> conva
                            in conv
                    return $ fmap fixconv erconv

solveRecursiveShimWits ::
       forall (ground :: GroundTypeKind) pola polb wit a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => (forall pa pb. DolanType ground pola pa -> DolanType ground polb pb -> Solver ground wit (DolanShim ground pa pb))
    -> PShimWit (DolanPolyIsoShim ground Type) (RecursiveOrPlainType ground) pola a
    -> PShimWit (DolanPolyIsoShim ground Type) (RecursiveOrPlainType ground) polb b
    -> Solver ground wit (DolanShim ground a b)
solveRecursiveShimWits solvePlainTypes (MkShimWit rpta conva) (MkShimWit rptb convb) =
    fmap (\conv -> polarPolyIsoNegative convb . conv . polarPolyIsoPositive conva) $
    solveRecursiveTypes solvePlainTypes rpta rptb

-- | at least one type must be recursive
solveRecursiveSingularTypes ::
       forall (ground :: GroundTypeKind) pola polb wit a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => (forall pa pb. DolanType ground pola pa -> DolanType ground polb pb -> Solver ground wit (DolanShim ground pa pb))
    -> DolanSingularType ground pola a
    -> DolanSingularType ground polb b
    -> Solver ground wit (DolanShim ground a b)
solveRecursiveSingularTypes solvePlainTypes ta tb =
    solveRecursiveShimWits solvePlainTypes (singularRecursiveOrPlainType ta) (singularRecursiveOrPlainType tb)
