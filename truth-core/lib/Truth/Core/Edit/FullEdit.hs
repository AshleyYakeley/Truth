module Truth.Core.Edit.FullEdit where

import Truth.Core.Edit.Edit
import Truth.Core.Import
import Truth.Core.Read

class (Edit edit, FullSubjectReader (EditReader edit)) =>
      FullEdit edit where
    replaceEdit :: WriterReadable edit (EditReader edit) ()

getReplaceEditsM ::
       forall m edit. (FullEdit edit, MonadIO m)
    => EditSubject edit
    -> m [edit]
getReplaceEditsM = fromReadableSubject (writerToReadable replaceEdit :: Readable (EditReader edit) [edit])
