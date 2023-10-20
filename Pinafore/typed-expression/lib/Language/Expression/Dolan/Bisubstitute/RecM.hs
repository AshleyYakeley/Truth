module Language.Expression.Dolan.Bisubstitute.RecM where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

data RecMemoKey (ground :: GroundTypeKind) (t :: (Type, Polarity, Type)) where
    MkRecMemoKey
        :: forall (ground :: GroundTypeKind) tv polarity a.
           TypeVarT tv
        -> PolarityType polarity
        -> DolanType ground polarity a
        -> RecMemoKey ground '( tv, polarity, a)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (RecMemoKey ground) where
    testEquality (MkRecMemoKey vara pola ta) (MkRecMemoKey varb polb tb) = do
        Refl <- testEquality vara varb
        Refl <- testEquality pola polb
        Refl <- withRepresentative pola $ testEquality ta tb
        return Refl

data RecMemoValue (ground :: GroundTypeKind) (pshim :: PolyShimKind) (t :: (Type, Polarity, Type)) where
    MkRecMemoValue
        :: forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) tv polarity a.
           (FuncShimWit (DolanSingularType ground polarity) (PolarShim (pshim Type) polarity) tv a)
        -> RecMemoValue ground pshim '( tv, polarity, a)

type RecMemoTable (ground :: GroundTypeKind) (pshim :: PolyShimKind)
     = WitnessMapFor (RecMemoValue ground pshim) (RecMemoKey ground)

type RecM (ground :: GroundTypeKind) (pshim :: PolyShimKind) = State (RecMemoTable ground pshim)

runRecM :: forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) a. RecM ground pshim a -> a
runRecM ra = evalState ra mempty

-- Turning this off will make recursive substitution slower
doMemoiseINTERNAL :: Bool
doMemoiseINTERNAL = True

memoiseRecM ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) tv polarity a.
       (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity a
    -> RecM ground pshim (FuncShimWit (DolanSingularType ground polarity) (PolarShim (pshim Type) polarity) tv a)
    -> RecM ground pshim (FuncShimWit (DolanSingularType ground polarity) (PolarShim (pshim Type) polarity) tv a)
memoiseRecM var t getval
    | doMemoiseINTERNAL = do
        let key = MkRecMemoKey var (polarityType @polarity) t
        memos <- get
        case witnessMapForLookup key memos of
            Just (MkRecMemoValue fsw) -> return fsw
            Nothing -> do
                fsw <- getval
                put $ witnessMapForAdd key (MkRecMemoValue fsw) memos
                return fsw
memoiseRecM _ _ getval = getval
