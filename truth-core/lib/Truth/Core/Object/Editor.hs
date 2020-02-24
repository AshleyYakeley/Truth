module Truth.Core.Object.Editor where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Resource

data Editor (update :: Type) r = forall editor. MkEditor
    { editorInit :: Object (UpdateEdit update) -> LifeCycleIO editor
    , editorUpdate :: editor -> Object (UpdateEdit update) -> ResourceContext -> NonEmpty update -> EditContext -> IO ()
    , editorTask :: Task ()
    , editorDo :: editor -> Object (UpdateEdit update) -> Task () -> LifeCycleIO r
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
    (MkEditor (ei1 :: Object (UpdateEdit update) -> LifeCycleIO editor1) eu1 et1 ed1) <*> (MkEditor (ei2 :: Object (UpdateEdit update) -> LifeCycleIO editor2) eu2 et2 ed2) = let
        editorInit :: Object (UpdateEdit update) -> LifeCycleIO (editor1, editor2)
        editorInit object = do
            e1 <- ei1 object
            e2 <- ei2 object
            return (e1, e2)
        editorUpdate ::
               (editor1, editor2)
            -> Object (UpdateEdit update)
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

subscribeEditor :: ResourceContext -> Subscriber update -> Editor update r -> LifeCycleIO r
subscribeEditor rc (MkResource (rr :: _ tt) (MkASubscriber anobject sub utask)) MkEditor {..} = do
    let object = MkResource rr anobject
    e <- editorInit object
    runResourceRunner rc rr $ sub editorTask $ editorUpdate e object
    editorDo e object utask
