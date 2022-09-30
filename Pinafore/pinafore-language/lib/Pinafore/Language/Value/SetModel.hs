module Pinafore.Language.Value.SetModel where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.Model
import Pinafore.Language.Value.WholeModel
import Shapes

data LangSetModel a =
    MkLangSetModel (a -> a -> Bool)
                   (WModel (PartialSetUpdate a))

mkLangSetModel :: Eq a => WModel (PartialSetUpdate a) -> LangSetModel a
mkLangSetModel sv = MkLangSetModel (==) sv

unLangSetModel :: LangSetModel a -> WModel (PartialSetUpdate a)
unLangSetModel (MkLangSetModel _ sv) = sv

instance Contravariant LangSetModel where
    contramap :: forall a b. (a -> b) -> LangSetModel b -> LangSetModel a
    contramap ab (MkLangSetModel eqb sv) = let
        eqa a1 a2 = eqb (ab a1) (ab a2)
        matchba b a = eqb b (ab a)
        mapset :: ReaderSet (SetReader b) -> ReaderSet (SetReader a)
        mapset rset (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
            rset $ MkTupleUpdateReader (MkFunctionSelector $ ab a) ReadWhole
        in MkLangSetModel eqa $ eaMap (partialiseChangeLens mapset (contramapPartialFunctionChangeLens ab matchba)) sv

instance MaybeRepresentational LangSetModel where
    maybeRepresentational = Nothing

instance HasVariance LangSetModel where
    type VarianceOf LangSetModel = 'Contravariance

langSetModelToModel :: forall a. LangSetModel a -> LangModel
langSetModelToModel (MkLangSetModel _ model) = MkLangModel model

langSetModelImmutable :: forall a. LangSetModel a -> LangSetModel a
langSetModelImmutable (MkLangSetModel eq sv) =
    MkLangSetModel eq $ eaMap (fromReadOnlyRejectingChangeLens . toReadOnlyChangeLens) sv

langSetModelComplement :: forall a. LangSetModel a -> LangSetModel a
langSetModelComplement (MkLangSetModel eq sv) = let
    mapset :: ReaderSet (UpdateReader (SetUpdate a)) -> ReaderSet (UpdateReader (SetUpdate a))
    mapset rset = rset
    in MkLangSetModel eq $ eaMap (liftPartialChangeLens mapset setUpdateComplement) sv

langSetModelCombine ::
       forall a.
       ChangeLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a)
    -> LangSetModel a
    -> LangSetModel a
    -> LangSetModel a
langSetModelCombine clens (MkLangSetModel eq1 sv1) (MkLangSetModel eq2 sv2) = let
    eq12 a b = eq1 a b || eq2 a b -- a bit odd, but the safest thing
    mapset :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate a)) -> ReaderSet (SetReader a)
    mapset rset (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
        (rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) ||
        (rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole)
    in MkLangSetModel eq12 $ eaMap (liftPartialChangeLens mapset clens . partialPairChangeLens) $ eaPair sv1 sv2

langSetModelIntersect :: forall a. LangSetModel a -> LangSetModel a -> LangSetModel a
langSetModelIntersect = langSetModelCombine setUpdateIntersection

langSetModelUnion :: forall a. LangSetModel a -> LangSetModel a -> LangSetModel a
langSetModelUnion = langSetModelCombine setUpdateUnion

langSetModelDifference :: forall a. LangSetModel a -> LangSetModel a -> LangSetModel a
langSetModelDifference = langSetModelCombine setUpdateDifference

langSetModelSymmetricDifference :: forall a. LangSetModel a -> LangSetModel a -> LangSetModel a
langSetModelSymmetricDifference = langSetModelCombine setUpdateSymmetricDifference

langSetModelCartesianSum :: forall a b. LangSetModel a -> LangSetModel b -> LangSetModel (Either a b)
langSetModelCartesianSum (MkLangSetModel eqA svA) (MkLangSetModel eqB svB) = let
    eqAB (Left a1) (Left a2) = eqA a1 a2
    eqAB (Right b1) (Right b2) = eqB b1 b2
    eqAB _ _ = False
    mapset :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate b)) -> ReaderSet (SetReader (Either a b))
    mapset rset (MkTupleUpdateReader (MkFunctionSelector eab) ReadWhole) =
        case eab of
            Left a -> rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
            Right b -> rset $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) ReadWhole
    in MkLangSetModel eqAB $
       eaMap (liftPartialChangeLens mapset setCartesianSumChangeLens . partialPairChangeLens) $ eaPair svA svB

langSetModelCartesianProduct :: forall a b. LangSetModel a -> LangSetModel b -> LangSetModel (a, b)
langSetModelCartesianProduct (MkLangSetModel eqA svA) (MkLangSetModel eqB svB) = let
    eqAB (a1, b1) (a2, b2) = eqA a1 a2 && eqB b1 b2
    in MkLangSetModel eqAB $
       eaMap (fromReadOnlyRejectingChangeLens . setCartesianProductPartialLens eqA eqB) $ eaPair svA svB

langSetModelAdd :: forall a. LangSetModel a -> a -> PinaforeAction ()
langSetModelAdd (MkLangSetModel _eq sv) a =
    pinaforeModelPush sv $ pure $ MkTupleUpdateEdit (MkFunctionSelector a) $ MkWholeReaderEdit True

langSetModelRemove :: forall a. LangSetModel a -> a -> PinaforeAction ()
langSetModelRemove (MkLangSetModel _eq sv) a =
    pinaforeModelPush sv $ pure $ MkTupleUpdateEdit (MkFunctionSelector a) $ MkWholeReaderEdit False

langSetModelMember :: forall a. LangSetModel a -> LangWholeModel '( BottomType, a) -> LangWholeModel '( Bool, Bool)
langSetModelMember (MkLangSetModel eq sv) aref = let
    afval = langWholeModelToReadOnlyValue aref
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
    in pinaforeModelToWholeModel $ eaMap knowApplySetLens $ eaPair sv $ eaMap fromReadOnlyRejectingChangeLens afval

predicateToLangSetModel :: forall a. (a -> Bool) -> LangSetModel (MeetType Entity a)
predicateToLangSetModel p = MkLangSetModel (==) $ eaMap fromReadOnlyRejectingChangeLens $ eaPure $ \mea -> p $ meet2 mea

predicateModelToLangSetModel ::
       forall a. LangWholeModel '( MeetType Entity a -> Bool, a -> Bool) -> LangSetModel (MeetType Entity a)
predicateModelToLangSetModel model = let
    sv = langWholeModelToValue $ coRangeLift (\s ma -> s $ meet2 ma) model
    in MkLangSetModel (==) $ eaMap (partialConvertChangeLens . unknownValueChangeLens (\_ -> False)) sv
