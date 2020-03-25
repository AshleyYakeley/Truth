module Truth.Core.Edit.FullEdit where

import Truth.Core.Edit.Edit
import Truth.Core.Import
import Truth.Core.Read

class (SubjectMapEdit edit, ApplicableEdit edit, FullSubjectReader (EditReader edit)) => FullEdit edit where
    replaceEdit ::
           forall m. (MonadIO m)
        => Readable m (EditReader edit)
        -> (edit -> m ())
        -> m ()

getReplaceEdits ::
       forall m edit. (FullEdit edit, MonadIO m)
    => Readable m (EditReader edit)
    -> m [edit]
getReplaceEdits mr = execWriterT $ replaceEdit (remonadReadable lift mr) $ tell . pure

getReplaceEditsFromSubject ::
       forall m edit. (FullEdit edit, MonadIO m)
    => EditSubject edit
    -> m [edit]
getReplaceEditsFromSubject subj = getReplaceEdits $ subjectToReadable subj
