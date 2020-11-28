module Changes.Core.Model.Editor where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model.EditContext
import Changes.Core.Model.Model
import Changes.Core.Model.Reference
import Changes.Core.Resource

data Editor (update :: Type) r = forall editor. MkEditor
    { editorInit :: Reference (UpdateEdit update) -> LifeCycle editor
    , editorUpdate :: editor -> Reference (UpdateEdit update) -> ResourceContext -> NonEmpty update -> EditContext -> IO ()
    , editorTask :: Task ()
    , editorDo :: editor -> Reference (UpdateEdit update) -> Task () -> LifeCycle r
    }

instance Functor (Editor update) where
    fmap ab (MkEditor ei eu et ed) = MkEditor ei eu et $ \e o ut -> fmap ab $ ed e o ut

instance Applicative (Editor update) where
    pure a = let
        editorInit _ = return ()
        editorUpdate () _ _ _ _ = return ()
        editorTask = mempty
        editorDo () _ _ = return a
        in MkEditor {..}
    (MkEditor (ei1 :: Reference (UpdateEdit update) -> LifeCycle editor1) eu1 et1 ed1) <*> (MkEditor (ei2 :: Reference (UpdateEdit update) -> LifeCycle editor2) eu2 et2 ed2) = let
        editorInit :: Reference (UpdateEdit update) -> LifeCycle (editor1, editor2)
        editorInit reference = do
            e1 <- ei1 reference
            e2 <- ei2 reference
            return (e1, e2)
        editorUpdate ::
               (editor1, editor2)
            -> Reference (UpdateEdit update)
            -> ResourceContext
            -> NonEmpty update
            -> EditContext
            -> IO ()
        editorUpdate (e1, e2) obj rc edits ectxt = do
            eu1 e1 obj rc edits ectxt
            eu2 e2 obj rc edits ectxt
        editorTask = et1 <> et2
        editorDo (e1, e2) obj utask = do
            ab <- ed1 e1 obj utask
            a <- ed2 e2 obj utask
            return $ ab a
        in MkEditor {..}

runEditor :: ResourceContext -> Model update -> Editor update r -> LifeCycle r
runEditor rc (MkResource (rr :: _ tt) (MkAModel anreference sub utask)) MkEditor {..} = do
    let reference = MkResource rr anreference
    e <- editorInit reference
    runResourceRunner rc rr $ sub editorTask $ editorUpdate e reference
    editorDo e reference utask
