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
    readFromSubjectM msubj (SumReadLeft reader) = readFromSubjectM msubj reader
    readFromSubjectM msubj (SumReadRight reader) = readFromSubjectM msubj reader
    readFromSubject subj (SumReadLeft reader) = readFromSubject subj reader
    readFromSubject subj (SumReadRight reader) = readFromSubject subj reader

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

instance (InvertableEdit ea, InvertableEdit eb, EditReader ea ~ EditReader eb) => InvertableEdit (SumEdit ea eb) where
    invertEdit (SumEditLeft edit) = fmap (fmap SumEditLeft) (invertEdit edit)
    invertEdit (SumEditRight edit) = fmap (fmap SumEditRight) (invertEdit edit)

instance (FullEdit ea, Edit eb, EditReader ea ~ EditReader eb) => FullEdit (SumEdit ea eb) where
    replaceEdit = reWriterReadable SumEditLeft replaceEdit

sumEditFunction :: (EditReader edit ~ EditReader edit') => PureEditFunction edit (SumEdit edit' edit)
sumEditFunction = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    editGet () rt = readable rt
    editUpdate edit () = return ((), [SumEditRight edit])
    in MkEditFunction {..}
