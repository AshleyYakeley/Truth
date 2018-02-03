module Truth.Core.Types.Tuple where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

class TupleWitness (c :: * -> Constraint) (sel :: * -> *) where
    tupleWitness :: forall proxy edit. proxy c -> sel edit -> Dict (c edit)

newtype Tuple sel =
    MkTuple (forall edit. sel edit -> EditSubject edit)

class (TestEquality sel, TupleReaderWitness SubjectReader sel) =>
      SubjectTupleSelector (sel :: * -> *) where
    type TupleSubject sel :: *
    type TupleSubject sel = Tuple sel
    tupleReadFromSubject :: forall edit. sel edit -> TupleSubject sel -> EditSubject edit
    default tupleReadFromSubject :: forall edit. (TupleSubject sel ~ Tuple sel) =>
                                                     sel edit -> TupleSubject sel -> EditSubject edit
    tupleReadFromSubject sel (MkTuple tuple) = tuple sel

data TupleEditReader sel t where
    MkTupleEditReader :: sel edit -> EditReader edit t -> TupleEditReader sel t

tupleReadFunction :: sel edit -> ReadFunction (TupleEditReader sel) (EditReader edit)
tupleReadFunction sel mr rt = mr $ MkTupleEditReader sel rt

instance (SubjectTupleSelector sel) => SubjectReader (TupleEditReader sel) where
    type ReaderSubject (TupleEditReader sel) = TupleSubject sel
    subjectToRead a (MkTupleEditReader seledit reader) =
        case tupleReaderWitness (Proxy :: Proxy SubjectReader) seledit of
            Dict -> subjectToRead (tupleReadFromSubject seledit a) reader

class TupleReaderWitness (c :: (* -> *) -> Constraint) (sel :: * -> *) where
    tupleReaderWitness :: forall proxy edit. proxy c -> sel edit -> Dict (c (EditReader edit))

class TupleSubjectWitness (c :: * -> Constraint) (sel :: * -> *) where
    tupleSubjectWitness :: forall proxy edit. proxy c -> sel edit -> Dict (c (EditSubject edit))

class TestEquality sel =>
      FiniteTupleSelector (sel :: * -> *) where
    tupleConstruct ::
           forall m. Applicative m
        => (forall edit. sel edit -> m (EditSubject edit))
        -> m (TupleSubject sel)

tupleAllSelectors :: FiniteTupleSelector sel => [AnyWitness sel]
tupleAllSelectors = getConst $ tupleConstruct $ \sel -> Const [MkAnyWitness sel]

instance (SubjectTupleSelector sel, FiniteTupleSelector sel, TupleReaderWitness FullSubjectReader sel) =>
         FullSubjectReader (TupleEditReader sel) where
    mutableReadToSubject mr =
        tupleConstruct $ \(seledit :: sel edit) ->
            case tupleReaderWitness (Proxy :: Proxy FullSubjectReader) seledit of
                Dict -> mutableReadToSubject $ mr . MkTupleEditReader seledit

data TupleEdit sel where
    MkTupleEdit :: sel edit -> edit -> TupleEdit sel

instance (TestEquality sel, TupleWitness Edit sel) => Floating (TupleEdit sel) (TupleEdit sel) where
    floatingUpdate (MkTupleEdit s1 e1) edit@(MkTupleEdit s2 e2) =
        case testEquality s1 s2 of
            Just Refl ->
                case tupleWitness (Proxy :: Proxy Edit) s2 of
                    Dict -> MkTupleEdit s2 $ floatingUpdate e1 e2
            Nothing -> edit

type instance EditReader (TupleEdit sel) = TupleEditReader sel

instance (TestEquality sel, TupleWitness Edit sel) => Edit (TupleEdit sel) where
    applyEdit (MkTupleEdit aggedite edit) mr aggreader@(MkTupleEditReader aggeditr reader) =
        case (tupleWitness (Proxy :: Proxy Edit) aggedite, testEquality aggedite aggeditr) of
            (Dict, Just Refl) -> applyEdit edit (mr . MkTupleEditReader aggedite) reader
            _ -> mr aggreader

instance (TestEquality sel, TupleWitness Edit sel, TupleWitness InvertibleEdit sel) =>
         InvertibleEdit (TupleEdit sel) where
    invertEdit (MkTupleEdit seledit edit) mr =
        case tupleWitness (Proxy :: Proxy InvertibleEdit) seledit of
            Dict -> fmap (fmap (MkTupleEdit seledit)) $ invertEdit edit $ mr . MkTupleEditReader seledit

instance ( SubjectTupleSelector sel
         , FiniteTupleSelector sel
         , TupleReaderWitness FullSubjectReader sel
         , TupleWitness Edit sel
         , TupleWitness FullEdit sel
         ) =>
         FullEdit (TupleEdit sel) where
    replaceEdit mr writeEdit = do
        editss <-
            for tupleAllSelectors $ \(MkAnyWitness sel) ->
                case tupleWitness (Proxy :: Proxy FullEdit) sel of
                    Dict -> replaceEdit (tupleReadFunction sel mr) $ writeEdit . MkTupleEdit sel
        return $ mconcat editss

splitTupleEditList :: TestEquality w => [TupleEdit w] -> AllF w []
splitTupleEditList [] = MkAllF $ \_ -> []
splitTupleEditList ((MkTupleEdit wt t):rr) =
    MkAllF $ \wt' ->
        case testEquality wt wt' of
            Just Refl -> t : (getAllF (splitTupleEditList rr) wt')
            Nothing -> getAllF (splitTupleEditList rr) wt'

tupleEditLens ::
       forall sel edit. (TestEquality sel)
    => sel edit
    -> EditLens (TupleEdit sel) edit
tupleEditLens seledit = let
    efGet :: ReadFunctionT IdentityT (TupleEditReader sel) (EditReader edit)
    efGet mr = remonadMutableRead IdentityT $ tupleReadFunction seledit mr
    efUpdate ::
           forall m. MonadIO m
        => TupleEdit sel
        -> MutableRead m (EditReader (TupleEdit sel))
        -> IdentityT m [edit]
    efUpdate (MkTupleEdit seledit' edit) _ =
        case testEquality seledit seledit' of
            Just Refl -> return [edit]
            Nothing -> return []
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => edit
        -> MutableRead m (EditReader (TupleEdit sel))
        -> IdentityT m (Maybe [TupleEdit sel])
    elPutEdit edit _ = return $ Just [MkTupleEdit seledit edit]
    in MkCloseUnlift identityUnlift $ MkAnEditLens {..}
