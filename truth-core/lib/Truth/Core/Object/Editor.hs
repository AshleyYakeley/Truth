module Truth.Core.Object.Editor where

import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber

data Editor (edit :: Type) actions r = forall editor. MkEditor
    { editorInit :: Object edit -> IO editor
    , editorUpdate :: editor -> Object edit -> [edit] -> IO ()
    , editorDo :: editor -> Object edit -> actions -> IO r
    }

instance Functor (Editor edit actions) where
    fmap ab (MkEditor ei eu ed) = MkEditor ei eu $ \e o a -> fmap ab $ ed e o a

instance Applicative (Editor edit actions) where
    pure a = let
        editorInit _ = return ()
        editorUpdate () _ _ = return ()
        editorDo () _ _ = return a
        in MkEditor {..}
    (MkEditor (ei1 :: Object edit -> IO editor1) eu1 ed1) <*> (MkEditor (ei2 :: Object edit -> IO editor2) eu2 ed2) = let
        editorInit :: Object edit -> IO (editor1, editor2)
        editorInit object = do
            e1 <- ei1 object
            e2 <- ei2 object
            return (e1, e2)
        editorUpdate :: (editor1, editor2) -> Object edit -> [edit] -> IO ()
        editorUpdate (e1, e2) obj edits = do
            eu1 e1 obj edits
            eu2 e2 obj edits
        editorDo (e1, e2) obj actions = do
            ab <- ed1 e1 obj actions
            a <- ed2 e2 obj actions
            return $ ab a
        in MkEditor {..}

subscribeEditor :: Subscriber edit actions -> Editor edit actions r -> IO r
subscribeEditor subscriber editor =
    case editor of
        MkEditor initr update f ->
            withLifeCycle (subscribe subscriber initr update) $ \(e, object, actions) -> f e object actions
{-
oneTransactionEditor ::
       forall actions edit r.
       (forall m. Monad m =>
                      MutableEdit m edit -> m r)
    -> Editor edit actions r
oneTransactionEditor f = let
    editorInit :: Object edit -> IO (Object edit)
    editorInit object = return object
    editorUpdate _lapiw _mr _edits = return ()
    editorDo (MkObject object) _ = object f
    in MkEditor {..}
-}
