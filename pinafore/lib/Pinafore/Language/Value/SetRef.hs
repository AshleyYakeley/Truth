module Pinafore.Language.Value.SetRef where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.OpenEntity
import Pinafore.Language.Value.Ref
import Shapes
import Truth.Core

data PinaforeSetRef a =
    MkPinaforeSetRef (a -> a -> Bool)
                     (PinaforeValue (PartialSetUpdate a))

mkPinaforeSetRef :: Eq a => PinaforeValue (PartialSetUpdate a) -> PinaforeSetRef a
mkPinaforeSetRef sv = MkPinaforeSetRef (==) sv

unPinaforeSetRef :: PinaforeSetRef a -> PinaforeValue (PartialSetUpdate a)
unPinaforeSetRef (MkPinaforeSetRef _ sv) = sv

instance Contravariant PinaforeSetRef where
    contramap :: forall a b. (a -> b) -> PinaforeSetRef b -> PinaforeSetRef a
    contramap ab (MkPinaforeSetRef eqb sv) = let
        eqa a1 a2 = eqb (ab a1) (ab a2)
        matchba b a = eqb b (ab a)
        mapset :: ReaderSet (SetReader b) -> ReaderSet (SetReader a)
        mapset rset (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
            rset $ MkTupleUpdateReader (MkFunctionSelector $ ab a) ReadWhole
        in MkPinaforeSetRef eqa $ eaMap (partialiseEditLens mapset (contramapPartialFunctionEditLens ab matchba)) sv

instance HasVariance 'Contravariance PinaforeSetRef where
    varianceRepresentational = Nothing

pinaforeSetRefImmutable :: forall a. PinaforeSetRef a -> PinaforeSetRef a
pinaforeSetRefImmutable (MkPinaforeSetRef eq sv) =
    MkPinaforeSetRef eq $ eaMap (fromReadOnlyRejectingEditLens . toReadOnlyEditLens) sv

pinaforeSetRefComplement :: forall a. PinaforeSetRef a -> PinaforeSetRef a
pinaforeSetRefComplement (MkPinaforeSetRef eq sv) = let
    mapset :: ReaderSet (UpdateReader (SetUpdate a)) -> ReaderSet (UpdateReader (SetUpdate a))
    mapset rset = rset
    in MkPinaforeSetRef eq $ eaMap (liftPartialEditLens mapset setUpdateComplement) sv

pinaforeSetRefCombine ::
       forall a.
       EditLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
    -> PinaforeSetRef a
    -> PinaforeSetRef a
    -> PinaforeSetRef a
pinaforeSetRefCombine clens (MkPinaforeSetRef eq1 sv1) (MkPinaforeSetRef eq2 sv2) = let
    eq12 a b = eq1 a b || eq2 a b -- a bit odd, but the safest thing
    mapset :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate a)) -> ReaderSet (SetReader a)
    mapset rset (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
        (rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) ||
        (rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole)
    in MkPinaforeSetRef eq12 $ eaMap (liftPartialEditLens mapset clens . partialPairEditLens) $ eaPair sv1 sv2

pinaforeSetRefIntersect :: forall a. PinaforeSetRef a -> PinaforeSetRef a -> PinaforeSetRef a
pinaforeSetRefIntersect = pinaforeSetRefCombine setUpdateIntersection

pinaforeSetRefUnion :: forall a. PinaforeSetRef a -> PinaforeSetRef a -> PinaforeSetRef a
pinaforeSetRefUnion = pinaforeSetRefCombine setUpdateUnion

pinaforeSetRefDifference :: forall a. PinaforeSetRef a -> PinaforeSetRef a -> PinaforeSetRef a
pinaforeSetRefDifference = pinaforeSetRefCombine setUpdateDifference

pinaforeSetRefSymmetricDifference :: forall a. PinaforeSetRef a -> PinaforeSetRef a -> PinaforeSetRef a
pinaforeSetRefSymmetricDifference = pinaforeSetRefCombine setUpdateSymmetricDifference

pinaforeSetRefCartesianSum :: forall a b. PinaforeSetRef a -> PinaforeSetRef b -> PinaforeSetRef (Either a b)
pinaforeSetRefCartesianSum (MkPinaforeSetRef eqA svA) (MkPinaforeSetRef eqB svB) = let
    eqAB (Left a1) (Left a2) = eqA a1 a2
    eqAB (Right b1) (Right b2) = eqB b1 b2
    eqAB _ _ = False
    mapset :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate b)) -> ReaderSet (SetReader (Either a b))
    mapset rset (MkTupleUpdateReader (MkFunctionSelector eab) ReadWhole) =
        case eab of
            Left a -> rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
            Right b -> rset $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) ReadWhole
    in MkPinaforeSetRef eqAB $
       eaMap (liftPartialEditLens mapset setCartesianSumEditLens . partialPairEditLens) $ eaPair svA svB

pinaforeSetRefCartesianProduct :: forall a b. PinaforeSetRef a -> PinaforeSetRef b -> PinaforeSetRef (a, b)
pinaforeSetRefCartesianProduct (MkPinaforeSetRef eqA svA) (MkPinaforeSetRef eqB svB) = let
    eqAB (a1, b1) (a2, b2) = eqA a1 a2 && eqB b1 b2
    in MkPinaforeSetRef eqAB $
       eaMap (fromReadOnlyRejectingEditLens . setCartesianProductPartialLens eqA eqB) $ eaPair svA svB

pinaforeSetRefAdd :: forall a. PinaforeSetRef a -> a -> PinaforeAction ()
pinaforeSetRefAdd (MkPinaforeSetRef _eq sv) a =
    pinaforeValuePushAction sv $ pure $ MkTupleUpdateEdit (MkFunctionSelector a) $ MkWholeReaderEdit True

pinaforeSetRefAddNew :: forall . PinaforeSetRef NewEntity -> PinaforeAction NewEntity
pinaforeSetRefAddNew set = do
    (MkNewEntity -> e) <- liftIO $ newKeyContainerItem @(FiniteSet Entity)
    pinaforeSetRefAdd set e
    return e

pinaforeSetRefRemove :: forall a. PinaforeSetRef a -> a -> PinaforeAction ()
pinaforeSetRefRemove (MkPinaforeSetRef _eq sv) a =
    pinaforeValuePushAction sv $ pure $ MkTupleUpdateEdit (MkFunctionSelector a) $ MkWholeReaderEdit False

pinaforeSetRefMember :: forall a. PinaforeSetRef a -> PinaforeRef '( BottomType, a) -> PinaforeRef '( Bool, Bool)
pinaforeSetRefMember (MkPinaforeSetRef eq sv) aref = let
    afval = pinaforeRefToReadOnlyValue aref
    knowApplySetLens :: EditLens (PairUpdate (PartialSetUpdate a) (WholeUpdate (Know a))) (WholeUpdate (Know Bool))
    knowApplySetLens = let
        getFunc ::
               forall m. MonadIO m
            => MutableRead m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a)))
            -> a
            -> m Bool
        getFunc mr a = mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        getArg ::
               forall m. MonadIO m
            => MutableRead m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a)))
            -> m (Know a)
        getArg mr = mr $ MkTupleUpdateReader SelectSecond ReadWhole
        elGet ::
               forall m. MonadIO m
            => MutableRead m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a)))
            -> MutableRead m (WholeReader (Know Bool))
        elGet mr ReadWhole = do
            ka <- getArg mr
            for ka $ getFunc mr
        elUpdate ::
               forall m. MonadIO m
            => PairUpdate (PartialSetUpdate a) (WholeUpdate (Know a))
            -> MutableRead m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a)))
            -> m [WholeUpdate (Know Bool)]
        elUpdate (MkTupleUpdate SelectFirst (KnownPartialUpdate (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate b)))) mr = do
            ka <- getArg mr
            return $
                case ka of
                    Known a'
                        | eq a a' -> pure $ MkWholeUpdate $ Known b
                    _ -> []
        elUpdate (MkTupleUpdate SelectFirst (UnknownPartialUpdate rset)) mr = do
            ka <- getArg mr
            case ka of
                Known a
                    | rset $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole -> do
                        b <- getFunc mr a
                        return $ pure $ MkWholeUpdate $ Known b
                _ -> return []
        elUpdate (MkTupleUpdate SelectSecond (MkWholeUpdate ka)) mr = do
            case ka of
                Known a -> do
                    b <- getFunc mr a
                    return $ pure $ MkWholeUpdate $ Known b
                Unknown -> return $ pure $ MkWholeUpdate Unknown
        elPutEdits ::
               forall m. MonadIO m
            => [WholeEdit (Know Bool)]
            -> MutableRead m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a)))
            -> m (Maybe [PairUpdateEdit (PartialSetUpdate a) (WholeUpdate (Know a))])
        elPutEdits edits mr =
            case lastWholeEdit edits of
                Nothing -> return $ Just []
                Just kb -> do
                    ka <- getArg mr
                    return $
                        case (ka, kb) of
                            (Unknown, Unknown) -> Just []
                            (Known a, Known b) ->
                                Just $
                                pure $
                                MkTupleUpdateEdit SelectFirst $
                                MkTupleUpdateEdit (MkFunctionSelector a) $ MkWholeReaderEdit b
                            _ -> Nothing
        in MkEditLens {..}
    in pinaforeValueToRef $ eaMap knowApplySetLens $ eaPair sv $ eaMap fromReadOnlyRejectingEditLens afval

pinaforePredicateToSetRef :: forall a. (a -> Bool) -> PinaforeSetRef (MeetType Entity a)
pinaforePredicateToSetRef p =
    MkPinaforeSetRef (==) $ eaMap fromReadOnlyRejectingEditLens $ eaPure $ \mea -> p $ meet2 mea

pinaforePredicateRefToSetRef ::
       forall a. PinaforeRef '( MeetType Entity a -> Bool, a -> Bool) -> PinaforeSetRef (MeetType Entity a)
pinaforePredicateRefToSetRef ref = let
    sv = pinaforeRefToValue $ coRangeLift (\s ma -> s $ meet2 ma) ref
    in MkPinaforeSetRef (==) $ eaMap (partialConvertEditLens . unknownValueEditLens (\_ -> False)) sv
