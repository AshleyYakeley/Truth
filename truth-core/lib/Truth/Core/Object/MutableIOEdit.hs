module Truth.Core.Object.MutableIOEdit where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.MutableEdit
import Truth.Core.Read
import Truth.Core.Types.None

data MutableIOReader edit t where
    ReadMutableIO :: MutableIOReader edit (MutableEdit IO edit)

instance SubjectReader (EditReader edit) => SubjectReader (MutableIOReader edit) where
    type ReaderSubject (MutableIOReader edit) = EditSubject edit
    readFromSubject subj ReadMutableIO = constantMutableEdit subj

instance FullSubjectReader (EditReader edit) => FullSubjectReader (MutableIOReader edit) where
    subjectFromReader = do
        muted <- readable ReadMutableIO
        liftIO $ unReadable subjectFromReader $ mutableRead muted

type MutableIOEdit edit = NoEdit (MutableIOReader edit)

mutableIOEditLens :: forall edit. PureEditLens (MutableIOEdit edit) edit
mutableIOEditLens = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    editGet :: () -> ReadFunction (MutableIOReader edit) (EditReader edit)
    editGet () reader = do
        muted <- readable ReadMutableIO
        liftIO $ mutableRead muted reader
    editUpdate :: MutableIOEdit edit -> () -> Readable (MutableIOReader edit) ((), [edit])
    editUpdate edit = never edit
    editLensFunction = MkEditFunction {..}
    editLensPutEdit :: () -> edit -> Readable (MutableIOReader edit) (Maybe ((), [MutableIOEdit edit]))
    editLensPutEdit () edit = do
        muted <- readable ReadMutableIO
        liftIO $ do
            maction <- mutableEdit muted [edit]
            case maction of
                Just action -> action
                Nothing -> fail "mutableIOEditLens: failed"
        return $ pure ((), [])
    in MkEditLens {..}

mutableIOLiftEditLens ::
       forall edita editb. Edit edita
    => PureEditLens edita editb
    -> PureEditLens (MutableIOEdit edita) (MutableIOEdit editb)
mutableIOLiftEditLens lens = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    editGet :: () -> ReadFunction (MutableIOReader edita) (MutableIOReader editb)
    editGet () ReadMutableIO = do
        muted <- readable ReadMutableIO
        return $ fixedMapMutableEdit lens muted
    editUpdate edita = never edita
    editLensFunction = MkEditFunction {..}
    editLensPutEdit () editb = never editb
    in MkEditLens {..}
