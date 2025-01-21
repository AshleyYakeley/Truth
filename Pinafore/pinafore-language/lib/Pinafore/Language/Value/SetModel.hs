module Pinafore.Language.Value.SetModel where

import Import
import Pinafore.Language.Value.Model
import Pinafore.Language.Value.WholeModel

data LangSetModel a
    = MkLangSetModel
        (Equivalence a)
        (WModel (PartialSetUpdate a))

mkLangSetModel :: Eq a => WModel (PartialSetUpdate a) -> LangSetModel a
mkLangSetModel sv = MkLangSetModel eqEquivalence sv

unLangSetModel :: LangSetModel a -> WModel (PartialSetUpdate a)
unLangSetModel (MkLangSetModel _ sv) = sv

instance Contravariant LangSetModel where
    contramap :: forall a b. (a -> b) -> LangSetModel b -> LangSetModel a
    contramap ab (MkLangSetModel eqb sv) = let
        eqa = contramap ab eqb
        matchba b a = equivalent eqb b (ab a)
        mapset :: ReaderSet (SetReader b) -> ReaderSet (SetReader a)
        mapset rset (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
            rset $ MkTupleUpdateReader (MkFunctionSelector $ ab a) ReadWhole
        in MkLangSetModel eqa $ eaMap (partialiseChangeLens mapset (contramapPartialFunctionChangeLens ab matchba)) sv

instance MaybeRepresentational LangSetModel where
    maybeRepresentational = Nothing

instance HasVariance LangSetModel where
    type VarianceOf LangSetModel = 'Contravariance

instance Eq a => IsInvertibleModel (LangSetModel a) where
    invertibleModelLens f (MkLangSetModel eq model) = fmap (MkLangSetModel eq) $ wInvertibleModelLens f model

langSetModelEquivalence :: forall a. LangSetModel a -> Equivalence a
langSetModelEquivalence (MkLangSetModel eqv _) = eqv

langSetModelToModel :: forall a. LangSetModel a -> LangModel
langSetModelToModel (MkLangSetModel _ model) = MkLangModel model

langSetModelImmutable :: forall a. LangSetModel a -> LangSetModel a
langSetModelImmutable (MkLangSetModel eq sv) =
    MkLangSetModel eq $ eaMap (fromReadOnlyRejectingChangeLens . toReadOnlyChangeLens) sv

langSetModelEmpty :: forall a. LangSetModel a
langSetModelEmpty = MkLangSetModel mempty $ eaPureRejecting $ \_ -> False

langSetModelFull :: forall a. LangSetModel a
langSetModelFull = MkLangSetModel mempty $ eaPureRejecting $ \_ -> True

langSetModelComplement :: forall a. LangSetModel a -> LangSetModel a
langSetModelComplement (MkLangSetModel eq sv) = let
    mapset :: ReaderSet (UpdateReader (SetUpdate a)) -> ReaderSet (UpdateReader (SetUpdate a))
    mapset rset = rset
    in MkLangSetModel eq $ eaMap (liftPartialChangeLens mapset setUpdateComplement) sv

langSetModelCombine ::
    forall a.
    ChangeLens (PairUpdate (SetUpdate a) (SetUpdate a)) (SetUpdate a) ->
    LangSetModel a ->
    LangSetModel a ->
    LangSetModel a
langSetModelCombine clens (MkLangSetModel eq1 sv1) (MkLangSetModel eq2 sv2) = let
    mapset :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate a)) -> ReaderSet (SetReader a)
    mapset rset (MkTupleUpdateReader (MkFunctionSelector a) ReadWhole) =
        (rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole)
            || (rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole)
    in MkLangSetModel (eq1 <> eq2) $ eaMap (liftPartialChangeLens mapset clens . partialPairChangeLens) $ eaPair sv1 sv2

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
    mapset :: ReaderSet (PairUpdateReader (SetUpdate a) (SetUpdate b)) -> ReaderSet (SetReader (Either a b))
    mapset rset (MkTupleUpdateReader (MkFunctionSelector eab) ReadWhole) =
        case eab of
            Left a -> rset $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
            Right b -> rset $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) ReadWhole
    in MkLangSetModel (eqA <+++> eqB)
        $ eaMap (liftPartialChangeLens mapset setCartesianSumChangeLens . partialPairChangeLens)
        $ eaPair svA svB

langSetModelCartesianProduct :: forall a b. LangSetModel a -> LangSetModel b -> LangSetModel (a, b)
langSetModelCartesianProduct (MkLangSetModel eqA svA) (MkLangSetModel eqB svB) =
    MkLangSetModel (eqA <***> eqB)
        $ eaMap (fromReadOnlyRejectingChangeLens . setCartesianProductPartialLens eqA eqB)
        $ eaPair svA svB

langSetModelAdd :: forall a. LangSetModel a -> a -> Action ()
langSetModelAdd (MkLangSetModel _eq sv) a =
    actionModelPush sv $ pure $ MkTupleUpdateEdit (MkFunctionSelector a) $ MkWholeReaderEdit True

langSetModelRemove :: forall a. LangSetModel a -> a -> Action ()
langSetModelRemove (MkLangSetModel _eq sv) a =
    actionModelPush sv $ pure $ MkTupleUpdateEdit (MkFunctionSelector a) $ MkWholeReaderEdit False

langSetModelMember :: forall a. LangSetModel a -> LangWholeModel '(BottomType, a) -> LangWholeModel '(Bool, Bool)
langSetModelMember (MkLangSetModel eq sv) aref = let
    afval = langWholeModelToReadOnlyValue aref
    knowApplySetLens :: ChangeLens (PairUpdate (PartialSetUpdate a) (WholeUpdate (Know a))) (WholeUpdate (Know Bool))
    knowApplySetLens = let
        getFunc :: forall m. Readable m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a))) -> a -> m Bool
        getFunc mr a = mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) ReadWhole
        getArg :: forall m. Readable m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a))) -> m (Know a)
        getArg mr = mr $ MkTupleUpdateReader SelectSecond ReadWhole
        clRead ::
            forall m.
            MonadIO m =>
            Readable m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a))) ->
            Readable m (WholeReader (Know Bool))
        clRead mr ReadWhole = do
            ka <- getArg mr
            for ka $ getFunc mr
        clUpdate ::
            forall m.
            MonadIO m =>
            PairUpdate (PartialSetUpdate a) (WholeUpdate (Know a)) ->
            Readable m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a))) ->
            m [WholeUpdate (Know Bool)]
        clUpdate (MkTupleUpdate SelectFirst (KnownPartialUpdate (MkTupleUpdate (MkFunctionSelector a) (MkWholeUpdate b)))) mr = do
            ka <- getArg mr
            return
                $ case ka of
                    Known a'
                        | equivalent eq a a' -> pure $ MkWholeUpdate $ Known b
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
            forall m.
            MonadIO m =>
            [WholeEdit (Know Bool)] ->
            Readable m (PairUpdateReader (PartialSetUpdate a) (WholeUpdate (Know a))) ->
            m (Maybe [PairUpdateEdit (PartialSetUpdate a) (WholeUpdate (Know a))])
        clPutEdits edits mr =
            case lastWholeEdit edits of
                Nothing -> return $ Just []
                Just kb -> do
                    ka <- getArg mr
                    return
                        $ case (ka, kb) of
                            (Unknown, Unknown) -> Just []
                            (Known a, Known b) ->
                                Just
                                    $ pure
                                    $ MkTupleUpdateEdit SelectFirst
                                    $ MkTupleUpdateEdit (MkFunctionSelector a)
                                    $ MkWholeReaderEdit b
                            _ -> Nothing
        in MkChangeLens{..}
    in wModelToWholeModel $ eaMap knowApplySetLens $ eaPair sv $ eaMap fromReadOnlyRejectingChangeLens afval

predicateToLangSetModel :: forall a. (a -> Bool) -> LangSetModel a
predicateToLangSetModel p =
    MkLangSetModel (contramap p eqEquivalence) $ eaMap fromReadOnlyRejectingChangeLens $ eaPure p

entityPredicateModelToLangSetModel ::
    forall a. LangWholeModel '(MeetType Entity a -> Bool, a -> Bool) -> LangSetModel (MeetType Entity a)
entityPredicateModelToLangSetModel model = let
    sv = langWholeModelToValue $ coRangeLift (\s ma -> s $ meet2 ma) model
    in MkLangSetModel eqEquivalence $ eaMap (partialConvertChangeLens . unknownValueChangeLens (\_ -> False)) sv

predicateModelToLangSetModel :: forall a. Equivalence a -> LangWholeModel '(a -> Bool, a -> Bool) -> LangSetModel a
predicateModelToLangSetModel eqv model = let
    sv = langWholeModelToValue model
    in MkLangSetModel eqv $ eaMap (partialSetConvertChangeLens eqv . unknownValueChangeLens (\_ -> False)) sv
