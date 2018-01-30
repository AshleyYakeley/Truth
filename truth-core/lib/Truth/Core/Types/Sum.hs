module Truth.Core.Types.Sum where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

data SumReader ra rb (t :: *)
    = SumReadLeft (ra t)
    | SumReadRight (rb t)

instance (SubjectReader ra, SubjectReader rb, ReaderSubject ra ~ ReaderSubject rb) =>
         SubjectReader (SumReader ra rb) where
    type ReaderSubject (SumReader ra rb) = ReaderSubject ra
    mSubjectToMutableRead msubj (SumReadLeft reader) = mSubjectToMutableRead msubj reader
    mSubjectToMutableRead msubj (SumReadRight reader) = mSubjectToMutableRead msubj reader
    subjectToRead subj (SumReadLeft reader) = subjectToRead subj reader
    subjectToRead subj (SumReadRight reader) = subjectToRead subj reader

data SumEdit ea eb
    = SumEditLeft ea
    | SumEditRight eb

instance (Floating ea ea, Floating eb eb) => Floating (SumEdit ea eb) (SumEdit ea eb) where
    floatingUpdate (SumEditLeft e1) (SumEditLeft e2) = SumEditLeft $ floatingUpdate e1 e2
    floatingUpdate (SumEditRight e1) (SumEditRight e2) = SumEditRight $ floatingUpdate e1 e2
    floatingUpdate _ t = t

instance (Edit ea, Edit eb, EditReader ea ~ EditReader eb) => Edit (SumEdit ea eb) where
    type EditReader (SumEdit ea eb) = EditReader ea
    applyEdit (SumEditLeft edit) = applyEdit edit
    applyEdit (SumEditRight edit) = applyEdit edit

instance (InvertibleEdit ea, InvertibleEdit eb, EditReader ea ~ EditReader eb) => InvertibleEdit (SumEdit ea eb) where
    invertEdit (SumEditLeft edit) mr = fmap (fmap SumEditLeft) $ invertEdit edit mr
    invertEdit (SumEditRight edit) mr = fmap (fmap SumEditRight) $ invertEdit edit mr

instance (FullEdit ea, Edit eb, EditReader ea ~ EditReader eb) => FullEdit (SumEdit ea eb) where
    replaceEdit mr write = replaceEdit mr (\edit -> write $ SumEditLeft edit)

sumRightEditFunction ::
       forall editl editr. (EditReader editl ~ EditReader editr)
    => EditFunction editr (SumEdit editl editr)
sumRightEditFunction = let
    efGet :: ReadFunctionT IdentityT (EditReader editr) (EditReader editr)
    efGet mr = remonadMutableRead lift mr
    efUpdate ::
           forall m. MonadIO m
        => editr
        -> MutableRead m (EditReader editr)
        -> IdentityT m [SumEdit editl editr]
    efUpdate er _ = return [SumEditRight er]
    in MkCloseUnlift identityUnlift MkAnEditFunction {..}
