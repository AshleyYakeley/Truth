module Truth.Core.Object.Editor where

import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber

data Editor (edit :: Type) r = forall editor. MkEditor
    { editorInit :: Object edit -> IO editor
    , editorUpdate :: editor -> Object edit -> [edit] -> EditSource -> IO ()
    , editorDo :: editor -> Object edit -> IO r
    }

instance Functor (Editor edit) where
    fmap ab (MkEditor ei eu ed) = MkEditor ei eu $ \e o -> fmap ab $ ed e o

instance Applicative (Editor edit) where
    pure a = let
        editorInit _ = return ()
        editorUpdate () _ _ _ = return ()
        editorDo () _ = return a
        in MkEditor {..}
    (MkEditor (ei1 :: Object edit -> IO editor1) eu1 ed1) <*> (MkEditor (ei2 :: Object edit -> IO editor2) eu2 ed2) = let
        editorInit :: Object edit -> IO (editor1, editor2)
        editorInit object = do
            e1 <- ei1 object
            e2 <- ei2 object
            return (e1, e2)
        editorUpdate :: (editor1, editor2) -> Object edit -> [edit] -> EditSource -> IO ()
        editorUpdate (e1, e2) obj edits esrc = do
            eu1 e1 obj edits esrc
            eu2 e2 obj edits esrc
        editorDo (e1, e2) obj = do
            ab <- ed1 e1 obj
            a <- ed2 e2 obj
            return $ ab a
        in MkEditor {..}

subscribeEditor :: Subscriber edit -> Editor edit r -> IO r
subscribeEditor (MkSubscriber object sub) editor = do
    case editor of
        MkEditor initr update f -> do
            e <- initr object
            withLifeCycle (sub $ update e object) $ \() -> f e object
