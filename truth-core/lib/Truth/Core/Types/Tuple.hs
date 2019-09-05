module Truth.Core.Types.Tuple where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

class TupleUpdateWitness (c :: Type -> Constraint) (sel :: Type -> Type) where
    tupleUpdateWitness :: forall update. sel update -> Dict (c update)

class TupleEditWitness (c :: Type -> Constraint) (sel :: Type -> Type) where
    tupleEditWitness :: forall update. sel update -> Dict (c (UpdateEdit update))

class TupleReaderWitness (c :: (Type -> Type) -> Constraint) (sel :: Type -> Type) where
    tupleReaderWitness :: forall update. sel update -> Dict (c (UpdateReader update))

class TupleSubjectWitness (c :: Type -> Constraint) (sel :: Type -> Type) where
    tupleSubjectWitness :: forall update. sel update -> Dict (c (UpdateSubject update))

newtype Tuple sel =
    MkTuple (forall update. sel update -> UpdateSubject update)

instance (TupleSubjectWitness Show sel, FiniteWitness sel) => Show (Tuple sel) where
    show (MkTuple f) = let
        showWit :: AnyW sel -> String
        showWit (MkAnyW se) =
            case tupleSubjectWitness @Show se of
                Dict -> show $ f se
        in "{" ++ intercalate "," (fmap showWit allWitnesses) ++ "}"

class (TestEquality sel, TupleReaderWitness SubjectReader sel) => SubjectTupleSelector (sel :: Type -> Type) where
    type TupleSubject sel :: Type
    type TupleSubject sel = Tuple sel
    tupleReadFromSubject :: forall update. sel update -> TupleSubject sel -> UpdateSubject update
    default tupleReadFromSubject ::
        forall update. (TupleSubject sel ~ Tuple sel) => sel update -> TupleSubject sel -> UpdateSubject update
    tupleReadFromSubject sel (MkTuple tuple) = tuple sel
    tupleWriteToSubject :: forall update. sel update -> UpdateSubject update -> TupleSubject sel -> TupleSubject sel
    default tupleWriteToSubject ::
        forall update.
            (TupleSubject sel ~ Tuple sel) => sel update -> UpdateSubject update -> TupleSubject sel -> TupleSubject sel
    tupleWriteToSubject sel element (MkTuple tuple) =
        MkTuple $ \sel' ->
            case testEquality sel sel' of
                Just Refl -> element
                Nothing -> tuple sel'

data TupleUpdateReader sel t where
    MkTupleUpdateReader :: sel update -> UpdateReader update t -> TupleUpdateReader sel t

instance TupleReaderWitness (WitnessConstraint c) sel => WitnessConstraint c (TupleUpdateReader sel) where
    witnessConstraint (MkTupleUpdateReader (se :: sel update) (rt :: UpdateReader update t)) =
        case tupleReaderWitness @(WitnessConstraint c) se of
            Dict ->
                case witnessConstraint @_ @c rt of
                    Dict -> Dict

instance (AllWitnessConstraint Show sel, TupleReaderWitness (AllWitnessConstraint Show) sel) =>
             Show (TupleUpdateReader sel t) where
    show (MkTupleUpdateReader (se :: sel update) (rt :: UpdateReader update t)) =
        showAllWitness se ++
        " " ++
        case tupleReaderWitness @(AllWitnessConstraint Show) se of
            Dict -> showAllWitness rt

instance (AllWitnessConstraint Show sel, TupleReaderWitness (AllWitnessConstraint Show) sel) =>
             AllWitnessConstraint Show (TupleUpdateReader sel) where
    allWitnessConstraint = Dict

tupleReadFunction :: sel update -> ReadFunction (TupleUpdateReader sel) (UpdateReader update)
tupleReadFunction sel mr rt = mr $ MkTupleUpdateReader sel rt

instance (SubjectTupleSelector sel) => SubjectReader (TupleUpdateReader sel) where
    type ReaderSubject (TupleUpdateReader sel) = TupleSubject sel
    subjectToRead a (MkTupleUpdateReader sel reader) =
        case tupleReaderWitness @SubjectReader sel of
            Dict -> subjectToRead (tupleReadFromSubject sel a) reader

class TestEquality sel => FiniteTupleSelector (sel :: Type -> Type) where
    tupleConstruct ::
           forall m. Applicative m
        => (forall update. sel update -> m (UpdateSubject update))
        -> m (TupleSubject sel)

tupleAllSelectors :: FiniteTupleSelector sel => [AnyW sel]
tupleAllSelectors = getConst $ tupleConstruct $ \sel -> Const [MkAnyW sel]

instance (SubjectTupleSelector sel, FiniteTupleSelector sel, TupleReaderWitness FullSubjectReader sel) =>
             FullSubjectReader (TupleUpdateReader sel) where
    mutableReadToSubject mr =
        tupleConstruct $ \(sel :: sel update) ->
            case tupleReaderWitness @FullSubjectReader sel of
                Dict -> mutableReadToSubject $ mr . MkTupleUpdateReader sel

data TupleUpdateEdit sel where
    MkTupleUpdateEdit :: sel update -> UpdateEdit update -> TupleUpdateEdit sel

data TupleUpdate sel where
    MkTupleUpdate :: sel update -> update -> TupleUpdate sel

instance TupleUpdateWitness IsUpdate sel => IsUpdate (TupleUpdate sel) where
    type UpdateEdit (TupleUpdate sel) = TupleUpdateEdit sel
    editUpdate (MkTupleUpdateEdit sel edit) =
        case tupleUpdateWitness @IsUpdate sel of
            Dict -> MkTupleUpdate sel $ editUpdate edit

instance (TupleUpdateWitness IsUpdate sel, TupleUpdateWitness IsEditUpdate sel) => IsEditUpdate (TupleUpdate sel) where
    updateEdit (MkTupleUpdate sel update) =
        case tupleUpdateWitness @IsEditUpdate sel of
            Dict -> MkTupleUpdateEdit sel $ updateEdit update

instance (TestEquality sel, TupleEditWitness ApplicableEdit sel) => Floating (TupleUpdateEdit sel) (TupleUpdateEdit sel) where
    floatingUpdate (MkTupleUpdateEdit s1 e1) edit@(MkTupleUpdateEdit s2 e2) =
        case testEquality s1 s2 of
            Just Refl ->
                case tupleEditWitness @ApplicableEdit s2 of
                    Dict -> MkTupleUpdateEdit s2 $ floatingUpdate e1 e2
            Nothing -> edit

type instance EditReader (TupleUpdateEdit sel) =
     TupleUpdateReader sel

instance (TestEquality sel, TupleEditWitness ApplicableEdit sel) => ApplicableEdit (TupleUpdateEdit sel) where
    applyEdit (MkTupleUpdateEdit aggedite edit) mr aggreader@(MkTupleUpdateReader aggeditr reader) =
        case (tupleEditWitness @ApplicableEdit aggedite, testEquality aggedite aggeditr) of
            (Dict, Just Refl) -> applyEdit edit (mr . MkTupleUpdateReader aggedite) reader
            _ -> mr aggreader

data TupleUpdateEditList sel where
    MkTupleUpdateEditList :: sel update -> [UpdateEdit update] -> TupleUpdateEditList sel

appendAssortedEdit :: TestEquality sel => TupleUpdateEdit sel -> [TupleUpdateEditList sel] -> [TupleUpdateEditList sel]
appendAssortedEdit (MkTupleUpdateEdit sel edit) [] = [MkTupleUpdateEditList sel [edit]]
appendAssortedEdit tedit@(MkTupleUpdateEdit sel edit) (MkTupleUpdateEditList sel' edits:rest) =
    case testEquality sel sel' of
        Just Refl -> (MkTupleUpdateEditList sel' (edit : edits)) : rest
        Nothing -> (MkTupleUpdateEditList sel' edits) : (appendAssortedEdit tedit rest)

assortEdits :: TestEquality sel => [TupleUpdateEdit sel] -> [TupleUpdateEditList sel]
assortEdits [] = []
assortEdits (edit:edits) = appendAssortedEdit edit $ assortEdits edits

instance (TestEquality sel, TupleEditWitness InvertibleEdit sel) => InvertibleEdit (TupleUpdateEdit sel) where
    invertEdits tedits mr = do
        invedits <-
            for (assortEdits tedits) $ \(MkTupleUpdateEditList sel edits) ->
                case tupleEditWitness @InvertibleEdit sel of
                    Dict -> fmap (fmap (MkTupleUpdateEdit sel)) $ invertEdits edits $ mr . MkTupleUpdateReader sel
        return $ mconcat invedits

instance (SubjectTupleSelector sel, TupleEditWitness SubjectMapEdit sel) => SubjectMapEdit (TupleUpdateEdit sel) where
    mapSubjectEdits =
        mapEditToMapEdits $ \(MkTupleUpdateEdit sel edit) subj ->
            case tupleEditWitness @SubjectMapEdit sel of
                Dict -> do
                    newelement <- mapSubjectEdits [edit] $ tupleReadFromSubject sel subj
                    return $ tupleWriteToSubject sel newelement subj

instance ( SubjectTupleSelector sel
         , FiniteTupleSelector sel
         , TupleReaderWitness FullSubjectReader sel
         , TupleEditWitness ApplicableEdit sel
         , TupleEditWitness SubjectMapEdit sel
         , TupleEditWitness FullEdit sel
         ) => FullEdit (TupleUpdateEdit sel) where
    replaceEdit mr writeEdit = do
        editss <-
            for tupleAllSelectors $ \(MkAnyW sel) ->
                case tupleEditWitness @FullEdit sel of
                    Dict -> replaceEdit (tupleReadFunction sel mr) $ writeEdit . MkTupleUpdateEdit sel
        return $ mconcat editss

data TupleUpdateEditCacheKey cache sel ct where
    MkTupleUpdateEditCacheKey
        :: sel update -> TupleUpdateEditCacheKey cache sel (cache (EditCacheKey cache (UpdateEdit update)))

instance TestEquality sel => TestEquality (TupleUpdateEditCacheKey cache sel) where
    testEquality (MkTupleUpdateEditCacheKey se1) (MkTupleUpdateEditCacheKey se2) = do
        Refl <- testEquality se1 se2
        return Refl

instance (TestEquality sel, TupleEditWitness CacheableEdit sel) => CacheableEdit (TupleUpdateEdit sel) where
    type EditCacheKey cache (TupleUpdateEdit sel) = TupleUpdateEditCacheKey cache sel
    editCacheAdd (MkTupleUpdateReader (sel :: sel update) rt) t =
        case tupleEditWitness @CacheableEdit sel of
            Dict -> subcacheModify (MkTupleUpdateEditCacheKey sel) $ editCacheAdd @(UpdateEdit update) rt t
    editCacheLookup (MkTupleUpdateReader (sel :: sel update) rt) cache =
        case tupleEditWitness @CacheableEdit sel of
            Dict -> do
                subcache <- cacheLookup (MkTupleUpdateEditCacheKey sel) cache
                editCacheLookup @(UpdateEdit update) rt subcache
    editCacheUpdate (MkTupleUpdateEdit (sel :: sel update) edit) =
        case tupleEditWitness @CacheableEdit sel of
            Dict -> subcacheModify (MkTupleUpdateEditCacheKey sel) $ editCacheUpdate @(UpdateEdit update) edit

instance (TupleEditWitness Show sel, AllWitnessConstraint Show sel) => Show (TupleUpdateEdit sel) where
    show (MkTupleUpdateEdit sel edit) =
        "tuple " ++
        showAllWitness sel ++
        " " ++
        case tupleEditWitness @Show sel of
            Dict -> show edit

instance (TupleUpdateWitness Show sel, AllWitnessConstraint Show sel) => Show (TupleUpdate sel) where
    show (MkTupleUpdate sel update) =
        "tuple " ++
        showAllWitness sel ++
        " " ++
        case tupleUpdateWitness @Show sel of
            Dict -> show update

tupleEditLens_ ::
       forall sel update. (forall a. sel a -> Maybe (update :~: a)) -> sel update -> EditLens (TupleUpdate sel) update
tupleEditLens_ tester sel = let
    ufGet :: ReadFunctionT IdentityT (TupleUpdateReader sel) (UpdateReader update)
    ufGet mr = remonadMutableRead IdentityT $ tupleReadFunction sel mr
    ufUpdate ::
           forall m. MonadIO m
        => TupleUpdate sel
        -> MutableRead m (TupleUpdateReader sel)
        -> IdentityT m [update]
    ufUpdate (MkTupleUpdate sel' update) _ =
        case tester sel' of
            Just Refl -> return [update]
            Nothing -> return []
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> MutableRead m (TupleUpdateReader sel)
        -> IdentityT m (Maybe [TupleUpdateEdit sel])
    elPutEdits edits _ = return $ Just $ fmap (MkTupleUpdateEdit sel) edits
    in MkCloseUnlift identityUnlift $ MkAnEditLens {..}

tupleEditLens ::
       forall sel update. (TestEquality sel)
    => sel update
    -> EditLens (TupleUpdate sel) update
tupleEditLens sel = tupleEditLens_ (testEquality sel) sel

tupleIsoLens ::
       forall sela selb.
       (forall update. sela update -> selb update)
    -> (forall update. selb update -> sela update)
    -> EditLens (TupleUpdate sela) (TupleUpdate selb)
tupleIsoLens ab ba = let
    ufGet :: ReadFunctionT IdentityT (TupleUpdateReader sela) (TupleUpdateReader selb)
    ufGet mr (MkTupleUpdateReader sel rt) = lift $ mr $ MkTupleUpdateReader (ba sel) rt
    ufUpdate ::
           forall m. MonadIO m
        => TupleUpdate sela
        -> MutableRead m (TupleUpdateReader sela)
        -> IdentityT m [TupleUpdate selb]
    ufUpdate (MkTupleUpdate sel update) _ = return [MkTupleUpdate (ab sel) update]
    elFunction :: AnUpdateFunction IdentityT (TupleUpdate sela) (TupleUpdate selb)
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [TupleUpdateEdit selb]
        -> MutableRead m (TupleUpdateReader sela)
        -> IdentityT m (Maybe [TupleUpdateEdit sela])
    elPutEdits edits _ = return $ Just $ fmap (\(MkTupleUpdateEdit sel edit) -> MkTupleUpdateEdit (ba sel) edit) edits
    in MkCloseUnlift identityUnlift MkAnEditLens {..}
