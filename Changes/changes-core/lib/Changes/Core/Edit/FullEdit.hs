module Changes.Core.Edit.FullEdit where

import Changes.Core.Edit.Edit
import Changes.Core.Import
import Changes.Core.Read

class (SubjectMapEdit edit, ApplicableEdit edit, FullSubjectReader (EditReader edit)) => FullEdit edit where
    replaceEdit ::
        forall m.
        MonadIO m =>
        Readable m (EditReader edit) ->
        (edit -> m ()) ->
        m ()

getReplaceEdits ::
    forall m edit.
    (FullEdit edit, MonadIO m) =>
    Readable m (EditReader edit) ->
    m [edit]
getReplaceEdits mr = execWriterT $ replaceEdit (hoistReadable lift mr) $ tell . pure

getReplaceEditsFromSubject ::
    forall m edit.
    (FullEdit edit, MonadIO m) =>
    EditSubject edit ->
    m [edit]
getReplaceEditsFromSubject subj = getReplaceEdits $ subjectToReadable subj
