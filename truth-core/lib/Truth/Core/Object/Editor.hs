module Truth.Core.Object.Editor where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Resource

data Editor (update :: Type) r = forall editor. MkEditor
    { editorInit :: Object (UpdateEdit update) -> LifeCycleIO editor
    , editorUpdate :: editor -> Object (UpdateEdit update) -> [update] -> EditContext -> IO ()
    , editorDo :: editor -> Object (UpdateEdit update) -> LifeCycleIO r
    }

instance Functor (Editor update) where
    fmap ab (MkEditor ei eu ed) = MkEditor ei eu $ \e o -> fmap ab $ ed e o

instance Applicative (Editor update) where
    pure a = let
        editorInit _ = return ()
        editorUpdate () _ _ _ = return ()
        editorDo () _ = return a
        in MkEditor {..}
    (MkEditor (ei1 :: Object (UpdateEdit update) -> LifeCycleIO editor1) eu1 ed1) <*> (MkEditor (ei2 :: Object (UpdateEdit update) -> LifeCycleIO editor2) eu2 ed2) = let
        editorInit :: Object (UpdateEdit update) -> LifeCycleIO (editor1, editor2)
        editorInit object = do
            e1 <- ei1 object
            e2 <- ei2 object
            return (e1, e2)
        editorUpdate :: (editor1, editor2) -> Object (UpdateEdit update) -> [update] -> EditContext -> IO ()
        editorUpdate (e1, e2) obj edits ectxt = do
            eu1 e1 obj edits ectxt
            eu2 e2 obj edits ectxt
        editorDo (e1, e2) obj = do
            ab <- ed1 e1 obj
            a <- ed2 e2 obj
            return $ ab a
        in MkEditor {..}

subscribeEditor :: Subscriber update -> Editor update r -> LifeCycleIO r
subscribeEditor (MkResource1 (rr :: _ tt) (MkASubscriber anobject sub)) editor = let
    object = MkResource1 rr anobject
    in case editor of
           MkEditor initr update f ->
               runResourceRunnerWith rr $ \run -> do
                   e <- initr object
                   remonad run $ sub $ update e object
                   f e object
