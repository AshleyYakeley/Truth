module Pinafore.Language.Value.SetRef where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.OpenEntity
import Pinafore.Language.Value.Ref
import Shapes
import Truth.Core

data PinaforeSetRef baseupdate a =
    MkPinaforeSetRef (a -> a -> Bool)
                     (PinaforeLensValue baseupdate (PartialSetUpdate a))

mkPinaforeSetRef :: Eq a => PinaforeLensValue baseupdate (PartialSetUpdate a) -> PinaforeSetRef baseupdate a
mkPinaforeSetRef lens = MkPinaforeSetRef (==) lens

unPinaforeSetRef :: PinaforeSetRef baseupdate a -> PinaforeLensValue baseupdate (PartialSetUpdate a)
unPinaforeSetRef (MkPinaforeSetRef _ lens) = lens

instance Contravariant (PinaforeSetRef baseupdate) where
    contramap :: forall a b. (a -> b) -> PinaforeSetRef baseupdate b -> PinaforeSetRef baseupdate a
    contramap ab (MkPinaforeSetRef eqb lens) = let
        eqa a1 a2 = eqb (ab a1) (ab a2)
        matchba b a = eqb b (ab a)
        mapset :: ReaderSet (SetReader b) -> ReaderSet (SetReader a)
        mapset rset (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
            rset $ MkTupleUpdateReader (MkFunctionSelector $ ab a) ReadWhole
        in MkPinaforeSetRef eqa $ partialiseEditLens mapset (contramapPartialFunctionEditLens ab matchba) . lens

instance HasVariance 'Contravariance (PinaforeSetRef baseupdate) where
    varianceRepresentational = Nothing

pinaforeSetRefImmutable :: forall baseupdate a. PinaforeSetRef baseupdate a -> PinaforeSetRef baseupdate a
pinaforeSetRefImmutable (MkPinaforeSetRef eq lens) = MkPinaforeSetRef eq $ readOnlyEditLens $ editLensFunction lens

pinaforeSetRefComplement :: forall baseupdate a. PinaforeSetRef baseupdate a -> PinaforeSetRef baseupdate a
pinaforeSetRefComplement (MkPinaforeSetRef eq lens) = let
    mapset :: ReaderSet (UpdateReader (SetUpdate a)) -> ReaderSet (UpdateReader (SetUpdate a))
    mapset rset = rset
    in MkPinaforeSetRef eq $ liftPartialEditLens mapset setUpdateComplement . lens

pinaforeSetRefCombine ::
       forall baseupdate a.
       EditLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
    -> PinaforeSetRef baseupdate a
    -> PinaforeSetRef baseupdate a
    -> PinaforeSetRef baseupdate a
pinaforeSetRefCombine clens (MkPinaforeSetRef eq1 lens1) (MkPinaforeSetRef eq2 lens2) = let
    eq12 a b = eq1 a b || eq2 a b -- a bit odd, but the safest thing
    mapset :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate a)) -> ReaderSet (SetReader a)
    mapset rset (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
        (rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) ||
        (rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole)
    in MkPinaforeSetRef eq12 $
       liftPartialEditLens mapset clens . partialPairEditLens . pairCombineEditLenses lens1 lens2

pinaforeSetRefIntersect ::
       forall baseupdate a. PinaforeSetRef baseupdate a -> PinaforeSetRef baseupdate a -> PinaforeSetRef baseupdate a
pinaforeSetRefIntersect = pinaforeSetRefCombine setUpdateIntersection

pinaforeSetRefUnion ::
       forall baseupdate a. PinaforeSetRef baseupdate a -> PinaforeSetRef baseupdate a -> PinaforeSetRef baseupdate a
pinaforeSetRefUnion = pinaforeSetRefCombine setUpdateUnion

pinaforeSetRefDifference ::
       forall baseupdate a. PinaforeSetRef baseupdate a -> PinaforeSetRef baseupdate a -> PinaforeSetRef baseupdate a
pinaforeSetRefDifference = pinaforeSetRefCombine setUpdateDifference

pinaforeSetRefSymmetricDifference ::
       forall baseupdate a. PinaforeSetRef baseupdate a -> PinaforeSetRef baseupdate a -> PinaforeSetRef baseupdate a
pinaforeSetRefSymmetricDifference = pinaforeSetRefCombine setUpdateSymmetricDifference

pinaforeSetRefCartesianSum ::
       forall baseupdate a b.
       PinaforeSetRef baseupdate a
    -> PinaforeSetRef baseupdate b
    -> PinaforeSetRef baseupdate (Either a b)
pinaforeSetRefCartesianSum (MkPinaforeSetRef eqA lensA) (MkPinaforeSetRef eqB lensB) = let
    eqAB (Left a1) (Left a2) = eqA a1 a2
    eqAB (Right b1) (Right b2) = eqB b1 b2
    eqAB _ _ = False
    mapset :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate b)) -> ReaderSet (SetReader (Either a b))
    mapset rset (MkTupleUpdateReader (MkFunctionSelector eab) ReadWhole) =
        case eab of
            Left a -> rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
            Right b -> rset $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) ReadWhole
    in MkPinaforeSetRef eqAB $
       liftPartialEditLens mapset setCartesianSumEditLens . partialPairEditLens . pairCombineEditLenses lensA lensB

pinaforeSetRefCartesianProduct ::
       forall baseupdate a b.
       PinaforeSetRef baseupdate a
    -> PinaforeSetRef baseupdate b
    -> PinaforeSetRef baseupdate (a, b)
pinaforeSetRefCartesianProduct (MkPinaforeSetRef eqA lensA) (MkPinaforeSetRef eqB lensB) = let
    eqAB (a1, b1) (a2, b2) = eqA a1 a2 && eqB b1 b2
    in MkPinaforeSetRef eqAB $
       readOnlyEditLens (setCartesianProductPartialUpdateFunction eqA eqB) . pairCombineEditLenses lensA lensB

pinaforeSetRefAdd :: forall baseupdate a. PinaforeSetRef baseupdate a -> a -> PinaforeAction baseupdate ()
pinaforeSetRefAdd (MkPinaforeSetRef _eq lens) a =
    pinaforeLensPush lens $ pure $ MkTupleUpdateEdit (MkFunctionSelector a) $ MkWholeReaderEdit True

pinaforeSetRefAddNew :: forall baseupdate. PinaforeSetRef baseupdate NewEntity -> PinaforeAction baseupdate NewEntity
pinaforeSetRefAddNew set = do
    (MkNewEntity -> e) <- liftIO $ newKeyContainerItem @(FiniteSet Entity)
    pinaforeSetRefAdd set e
    return e

pinaforeSetRefRemove :: forall baseupdate a. PinaforeSetRef baseupdate a -> a -> PinaforeAction baseupdate ()
pinaforeSetRefRemove (MkPinaforeSetRef _eq lens) a =
    pinaforeLensPush lens $ pure $ MkTupleUpdateEdit (MkFunctionSelector a) $ MkWholeReaderEdit False

pinaforeSetRefMember :: forall baseupdate a. PinaforeSetRef baseupdate a -> a -> PinaforeRef baseupdate '( Bool, Bool)
pinaforeSetRefMember (MkPinaforeSetRef eq lens) a = let
    mapset :: ReaderSet (SetReader a) -> ReaderSet (WholeReader Bool)
    mapset _rset ReadWhole = True
    in pinaforeLensToRef $
       wholeEditLens knowMaybeLens .
       partialFullEditLens .
       (liftPartialEditLens mapset $
        tupleEditLens_
            (\(MkFunctionSelector a') ->
                 if eq a a'
                     then Just Refl
                     else Nothing) $
        MkFunctionSelector a) .
       lens

pinaforePredicateToSetRef :: forall baseupdate a. (a -> Bool) -> PinaforeSetRef baseupdate (MeetType Entity a)
pinaforePredicateToSetRef p = MkPinaforeSetRef (==) $ constEditLens $ \mea -> p $ meet2 mea

pinaforePredicateRefToSetRef ::
       forall baseupdate a.
       PinaforeRef baseupdate '( MeetType Entity a -> Bool, a -> Bool)
    -> PinaforeSetRef baseupdate (MeetType Entity a)
pinaforePredicateRefToSetRef ref = let
    lens = pinaforeRefToLens $ coRangeLift (\s ma -> s $ meet2 ma) ref
    in MkPinaforeSetRef (==) $ partialConvertEditLens . unknownValueEditLens (\_ -> False) . lens
