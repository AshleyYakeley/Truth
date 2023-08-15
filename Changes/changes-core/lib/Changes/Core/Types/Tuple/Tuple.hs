module Changes.Core.Types.Tuple.Tuple where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read

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
        showWit :: Some sel -> String
        showWit (MkSome se) =
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

instance (AllConstraint Show sel, TupleReaderWitness (AllConstraint Show) sel) => Show (TupleUpdateReader sel t) where
    show (MkTupleUpdateReader (se :: sel update) (rt :: UpdateReader update t)) =
        allShow se ++
        " " ++
        case tupleReaderWitness @(AllConstraint Show) se of
            Dict -> allShow rt

instance (AllConstraint Show sel, TupleReaderWitness (AllConstraint Show) sel) =>
             AllConstraint Show (TupleUpdateReader sel) where
    allConstraint = Dict

tupleReadFunction :: sel update -> ReadFunction (TupleUpdateReader sel) (UpdateReader update)
tupleReadFunction sel mr rt = mr $ MkTupleUpdateReader sel rt

instance (SubjectTupleSelector sel) => SubjectReader (TupleUpdateReader sel) where
    type ReaderSubject (TupleUpdateReader sel) = TupleSubject sel
    subjectToRead a (MkTupleUpdateReader sel rd) =
        case tupleReaderWitness @SubjectReader sel of
            Dict -> subjectToRead (tupleReadFromSubject sel a) rd

class TestEquality sel => FiniteTupleSelector (sel :: Type -> Type) where
    tupleConstruct ::
           forall m. Applicative m
        => (forall update. sel update -> m (UpdateSubject update))
        -> m (TupleSubject sel)

tupleAllSelectors :: FiniteTupleSelector sel => [Some sel]
tupleAllSelectors = getConst $ tupleConstruct $ \sel -> Const [MkSome sel]

instance (SubjectTupleSelector sel, FiniteTupleSelector sel, TupleReaderWitness FullSubjectReader sel) =>
             FullSubjectReader (TupleUpdateReader sel) where
    readableToSubject mr =
        tupleConstruct $ \(sel :: sel update) ->
            case tupleReaderWitness @FullSubjectReader sel of
                Dict -> readableToSubject $ mr . MkTupleUpdateReader sel

data TupleUpdateEdit sel where
    MkTupleUpdateEdit :: sel update -> UpdateEdit update -> TupleUpdateEdit sel

data TupleUpdate sel where
    MkTupleUpdate :: sel update -> update -> TupleUpdate sel

type instance UpdateEdit (TupleUpdate sel) = TupleUpdateEdit sel

instance TupleUpdateWitness IsUpdate sel => IsUpdate (TupleUpdate sel) where
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

type instance EditReader (TupleUpdateEdit sel) = TupleUpdateReader sel

instance (TestEquality sel, TupleEditWitness ApplicableEdit sel) => ApplicableEdit (TupleUpdateEdit sel) where
    applyEdit (MkTupleUpdateEdit aggedite edit) mr aggreader@(MkTupleUpdateReader aggeditr rd) =
        case (tupleEditWitness @ApplicableEdit aggedite, testEquality aggedite aggeditr) of
            (Dict, Just Refl) -> applyEdit edit (mr . MkTupleUpdateReader aggedite) rd
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
            for tupleAllSelectors $ \(MkSome sel) ->
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
    trimEdits = id
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

instance (TupleEditWitness Show sel, AllConstraint Show sel) => Show (TupleUpdateEdit sel) where
    show (MkTupleUpdateEdit sel edit) =
        "tuple " ++
        allShow sel ++
        " " ++
        case tupleEditWitness @Show sel of
            Dict -> show edit

instance (TupleUpdateWitness Show sel, AllConstraint Show sel) => Show (TupleUpdate sel) where
    show (MkTupleUpdate sel update) =
        "tuple " ++
        allShow sel ++
        " " ++
        case tupleUpdateWitness @Show sel of
            Dict -> show update

tupleChangeLens_ ::
       forall sel update. (forall a. sel a -> Maybe (update :~: a)) -> sel update -> ChangeLens (TupleUpdate sel) update
tupleChangeLens_ tester sel = let
    clRead :: ReadFunction (TupleUpdateReader sel) (UpdateReader update)
    clRead mr = tupleReadFunction sel mr
    clUpdate ::
           forall m. MonadIO m
        => TupleUpdate sel
        -> Readable m (TupleUpdateReader sel)
        -> m [update]
    clUpdate (MkTupleUpdate sel' update) _ =
        case tester sel' of
            Just Refl -> return [update]
            Nothing -> return []
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> Readable m (TupleUpdateReader sel)
        -> m (Maybe [TupleUpdateEdit sel])
    clPutEdits edits _ = return $ Just $ fmap (MkTupleUpdateEdit sel) edits
    in MkChangeLens {..}

tupleChangeLens ::
       forall sel update. (TestEquality sel)
    => sel update
    -> ChangeLens (TupleUpdate sel) update
tupleChangeLens sel = tupleChangeLens_ (testEquality sel) sel

tupleIsoLens ::
       forall sela selb.
       (forall update. sela update -> selb update)
    -> (forall update. selb update -> sela update)
    -> ChangeLens (TupleUpdate sela) (TupleUpdate selb)
tupleIsoLens ab ba = let
    clRead :: ReadFunction (TupleUpdateReader sela) (TupleUpdateReader selb)
    clRead mr (MkTupleUpdateReader sel rt) = mr $ MkTupleUpdateReader (ba sel) rt
    clUpdate ::
           forall m. MonadIO m
        => TupleUpdate sela
        -> Readable m (TupleUpdateReader sela)
        -> m [TupleUpdate selb]
    clUpdate (MkTupleUpdate sel update) _ = return [MkTupleUpdate (ab sel) update]
    clPutEdits ::
           forall m. MonadIO m
        => [TupleUpdateEdit selb]
        -> Readable m (TupleUpdateReader sela)
        -> m (Maybe [TupleUpdateEdit sela])
    clPutEdits edits _ = return $ Just $ fmap (\(MkTupleUpdateEdit sel edit) -> MkTupleUpdateEdit (ba sel) edit) edits
    in MkChangeLens {..}
