module Truth.Core.Object.Editor where

import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Object.UnliftIO

data Editor (edit :: Type) r = forall editor. MkEditor
    { editorInit :: Object edit -> LifeCycleIO editor
    , editorUpdate :: editor -> Object edit -> [edit] -> EditContext -> IO ()
    , editorDo :: editor -> Object edit -> LifeCycleIO r
    }

instance Functor (Editor edit) where
    fmap ab (MkEditor ei eu ed) = MkEditor ei eu $ \e o -> fmap ab $ ed e o

instance Applicative (Editor edit) where
    pure a = let
        editorInit _ = return ()
        editorUpdate () _ _ _ = return ()
        editorDo () _ = return a
        in MkEditor {..}
    (MkEditor (ei1 :: Object edit -> LifeCycleIO editor1) eu1 ed1) <*> (MkEditor (ei2 :: Object edit -> LifeCycleIO editor2) eu2 ed2) = let
        editorInit :: Object edit -> LifeCycleIO (editor1, editor2)
        editorInit object = do
            e1 <- ei1 object
            e2 <- ei2 object
            return (e1, e2)
        editorUpdate :: (editor1, editor2) -> Object edit -> [edit] -> EditContext -> IO ()
        editorUpdate (e1, e2) obj edits ectxt = do
            eu1 e1 obj edits ectxt
            eu2 e2 obj edits ectxt
        editorDo (e1, e2) obj = do
            ab <- ed1 e1 obj
            a <- ed2 e2 obj
            return $ ab a
        in MkEditor {..}

subscribeEditor :: Subscriber edit -> Editor edit r -> LifeCycleIO r
subscribeEditor (MkCloseUnliftIO run (MkASubscriber anobject sub)) editor = let
    object = MkCloseUnliftIO run anobject
    in case editor of
           MkEditor initr update f -> do
               e <- initr object
               remonad (runTransform run) $ sub $ update e object
               f e object
