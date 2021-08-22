module Pinafore.Language.Value.SetRef where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.WholeRef
import Shapes

data LangSetRef a =
    MkLangSetRef (a -> a -> Bool)
                 (WModel (PartialSetUpdate a))

mkLangSetRef :: Eq a => WModel (PartialSetUpdate a) -> LangSetRef a
mkLangSetRef sv = MkLangSetRef (==) sv

unLangSetRef :: LangSetRef a -> WModel (PartialSetUpdate a)
unLangSetRef (MkLangSetRef _ sv) = sv

instance Contravariant LangSetRef where
    contramap :: forall a b. (a -> b) -> LangSetRef b -> LangSetRef a
    contramap ab (MkLangSetRef eqb sv) = let
        eqa a1 a2 = eqb (ab a1) (ab a2)
        matchba b a = eqb b (ab a)
        mapset :: ReaderSet (SetReader b) -> ReaderSet (SetReader a)
        mapset rset (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
            rset $ MkTupleUpdateReader (MkFunctionSelector $ ab a) ReadWhole
        in MkLangSetRef eqa $ eaMap (partialiseChangeLens mapset (contramapPartialFunctionChangeLens ab matchba)) sv

instance MaybeRepresentational LangSetRef where
    maybeRepresentational = Nothing

instance HasVariance LangSetRef where
    type VarianceOf LangSetRef = 'Contravariance

langSetRefImmutable :: forall a. LangSetRef a -> LangSetRef a
langSetRefImmutable (MkLangSetRef eq sv) =
    MkLangSetRef eq $ eaMap (fromReadOnlyRejectingChangeLens . toReadOnlyChangeLens) sv

langSetRefComplement :: forall a. LangSetRef a -> LangSetRef a
langSetRefComplement (MkLangSetRef eq sv) = let
    mapset :: ReaderSet (UpdateReader (SetUpdate a)) -> ReaderSet (UpdateReader (SetUpdate a))
    mapset rset = rset
    in MkLangSetRef eq $ eaMap (liftPartialChangeLens mapset setUpdateComplement) sv

langSetRefCombine ::
       forall a.
       ChangeLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
    -> LangSetRef a
    -> LangSetRef a
    -> LangSetRef a
langSetRefCombine clens (MkLangSetRef eq1 sv1) (MkLangSetRef eq2 sv2) = let
    eq12 a b = eq1 a b || eq2 a b -- a bit odd, but the safest thing
    mapset :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate a)) -> ReaderSet (SetReader a)
    mapset rset (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
        (rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) ||
        (rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole)
    in MkLangSetRef eq12 $ eaMap (liftPartialChangeLens mapset clens . partialPairChangeLens) $ eaPair sv1 sv2

langSetRefIntersect :: forall a. LangSetRef a -> LangSetRef a -> LangSetRef a
langSetRefIntersect = langSetRefCombine setUpdateIntersection

langSetRefUnion :: forall a. LangSetRef a -> LangSetRef a -> LangSetRef a
langSetRefUnion = langSetRefCombine setUpdateUnion

langSetRefDifference :: forall a. LangSetRef a -> LangSetRef a -> LangSetRef a
langSetRefDifference = langSetRefCombine setUpdateDifference

langSetRefSymmetricDifference :: forall a. LangSetRef a -> LangSetRef a -> LangSetRef a
langSetRefSymmetricDifference = langSetRefCombine setUpdateSymmetricDifference

langSetRefCartesianSum :: forall a b. LangSetRef a -> LangSetRef b -> LangSetRef (Either a b)
langSetRefCartesianSum (MkLangSetRef eqA svA) (MkLangSetRef eqB svB) = let
    eqAB (Left a1) (Left a2) = eqA a1 a2
    eqAB (Right b1) (Right b2) = eqB b1 b2
    eqAB _ _ = False
    mapset :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate b)) -> ReaderSet (SetReader (Either a b))
    mapset rset (MkTupleUpdateReader (MkFunctionSelector eab) ReadWhole) =
        case eab of
            Left a -> rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
            Right b -> rset $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) ReadWhole
    in MkLangSetRef eqAB $
       eaMap (liftPartialChangeLens mapset setCartesianSumChangeLens . partialPairChangeLens) $ eaPair svA svB

langSetRefCartesianProduct :: forall a b. LangSetRef a -> LangSetRef b -> LangSetRef (a, b)
langSetRefCartesianProduct (MkLangSetRef eqA svA) (MkLangSetRef eqB svB) = let
    eqAB (a1, b1) (a2, b2) = eqA a1 a2 && eqB b1 b2
    in MkLangSetRef eqAB $
       eaMap (fromReadOnlyRejectingChangeLens . setCartesianProductPartialLens eqA eqB) $ eaPair svA svB

langSetRefAdd :: forall a. LangSetRef a -> a -> PinaforeAction ()
langSetRefAdd (MkLangSetRef _eq sv) a =
    pinaforeRefPush sv $ pure $ MkTupleUpdateEdit (MkFunctionSelector a) $ MkWholeReaderEdit True

langSetRefRemove :: forall a. LangSetRef a -> a -> PinaforeAction ()
langSetRefRemove (MkLangSetRef _eq sv) a =
    pinaforeRefPush sv $ pure $ MkTupleUpdateEdit (MkFunctionSelector a) $ MkWholeReaderEdit False

langSetRefMember :: forall a. LangSetRef a -> LangWholeRef '( BottomType, a) -> LangWholeRef '( Bool, Bool)
langSetRefMember (MkLangSetRef eq sv) aref = let
    afval = langWholeRefToReadOnlyValue aref
    knowApplySetLens :: ChangeLens (PairUpdate (PartialSetUpdate a) (WholeUpdate (Know a))) (WholeUpdate (Know Bool))
    knowApplySetLens = let
        getFunc ::
               forall m. MonadIO m
            => Readable m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a)))
            -> a
            -> m Bool
        getFunc mr a = mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        getArg ::
               forall m. MonadIO m
            => Readable m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a)))
            -> m (Know a)
        getArg mr = mr $ MkTupleUpdateReader SelectSecond ReadWhole
        clRead ::
               forall m. MonadIO m
            => Readable m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a)))
            -> Readable m (WholeReader (Know Bool))
        clRead mr ReadWhole = do
            ka <- getArg mr
            for ka $ getFunc mr
        clUpdate ::
               forall m. MonadIO m
            => PairUpdate (PartialSetUpdate a) (WholeUpdate (Know a))
            -> Readable m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a)))
            -> m [WholeUpdate (Know Bool)]
        clUpdate (MkTupleUpdate SelectFirst (KnownPartialUpdate (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate b)))) mr = do
            ka <- getArg mr
            return $
                case ka of
                    Known a'
                        | eq a a' -> pure $ MkWholeUpdate $ Known b
                    _ -> []
        clUpdate (MkTupleUpdate SelectFirst (UnknownPartialUpdate rset)) mr = do
            ka <- getArg mr
            case ka of
                Known a
                    | rset $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole -> do
                        b <- getFunc mr a
                        return $ pure $ MkWholeUpdate $ Known b
                _ -> return []
        clUpdate (MkTupleUpdate SelectSecond (MkWholeUpdate ka)) mr = do
            case ka of
                Known a -> do
                    b <- getFunc mr a
                    return $ pure $ MkWholeUpdate $ Known b
                Unknown -> return $ pure $ MkWholeUpdate Unknown
        clPutEdits ::
               forall m. MonadIO m
            => [WholeEdit (Know Bool)]
            -> Readable m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a)))
            -> m (Maybe [PairUpdateEdit (PartialSetUpdate a) (WholeUpdate (Know a))])
        clPutEdits edits mr =
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
        in MkChangeLens {..}
    in pinaforeRefToWholeRef $ eaMap knowApplySetLens $ eaPair sv $ eaMap fromReadOnlyRejectingChangeLens afval

predicateToLangSetRef :: forall a. (a -> Bool) -> LangSetRef (MeetType Entity a)
predicateToLangSetRef p = MkLangSetRef (==) $ eaMap fromReadOnlyRejectingChangeLens $ eaPure $ \mea -> p $ meet2 mea

predicateRefToLangSetRef ::
       forall a. LangWholeRef '( MeetType Entity a -> Bool, a -> Bool) -> LangSetRef (MeetType Entity a)
predicateRefToLangSetRef ref = let
    sv = langWholeRefToValue $ coRangeLift (\s ma -> s $ meet2 ma) ref
    in MkLangSetRef (==) $ eaMap (partialConvertChangeLens . unknownValueChangeLens (\_ -> False)) sv
