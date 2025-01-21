module Changes.Core.UI.Editor where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model
import Changes.Core.UI.View.View

type Editing :: Type -> Type -> Type
data Editing update r = MkEditing
    { editingUpdate :: NonEmpty update -> EditContext -> View ()
    , editingTask :: Task IO ()
    , editingDo :: Task IO () -> View r
    }

instance Functor (Editing update) where
    fmap ab (MkEditing eu et ed) = MkEditing eu et $ \ut -> fmap ab $ ed ut

instance Applicative (Editing update) where
    pure a = let
        editingUpdate _ _ = return ()
        editingTask = mempty
        editingDo _ = return a
        in MkEditing{..}
    (MkEditing eu1 et1 ed1) <*> (MkEditing eu2 et2 ed2) = let
        editingUpdate :: NonEmpty update -> EditContext -> View ()
        editingUpdate edits ec = do
            eu1 edits ec
            eu2 edits ec
        editingTask = et1 <> et2
        editingDo utask = do
            ab <- ed1 utask
            a <- ed2 utask
            return $ ab a
        in MkEditing{..}

type Editor :: Type -> Type -> Type
newtype Editor update r
    = MkEditor (Reference (UpdateEdit update) -> View (Editing update r))

instance Functor (Editor update) where
    fmap ab (MkEditor ed) = MkEditor $ fmap (fmap (fmap ab)) ed

instance Applicative (Editor update) where
    pure a = MkEditor $ pure $ pure $ pure a
    (MkEditor ed1) <*> (MkEditor ed2) = MkEditor $ liftA2 (liftA2 (<*>)) ed1 ed2

runEditor :: Model update -> Editor update r -> View r
runEditor model (MkEditor ed) = do
    e <- viewBindModelUpdates model (\_ -> True) (ed $ modelReference model) editingTask editingUpdate
    editingDo e $ modelUpdatesTask model

execEditor :: View (Editor update r) -> Editor update r
execEditor cv =
    MkEditor $ \ref -> do
        MkEditor ed <- cv
        ed ref

mapEditor :: forall updateA updateB r. ChangeLens updateA updateB -> Editor updateB r -> Editor updateA r
mapEditor l (MkEditor editor) =
    MkEditor $ \refA -> do
        MkEditing euB et ed <- editor $ mapReference l refA
        let
            euA :: NonEmpty updateA -> EditContext -> View ()
            euA updatesA ec =
                viewRunResourceContext refA $ \unlift arefA -> do
                    updatessB <-
                        for (toList updatesA) $ \updateA ->
                            clUpdate l updateA $ \rd -> liftIO $ unlift $ refRead arefA rd
                    case nonEmpty $ mconcat updatessB of
                        Nothing -> return ()
                        Just updatesB -> euB updatesB ec
        return $ MkEditing euA et ed

floatingMapEditor ::
    forall updateA updateB r. FloatingChangeLens updateA updateB -> Editor updateB r -> Editor updateA r
floatingMapEditor (MkFloatingChangeLens (NoFloatInit r) rlens) editorB = mapEditor (rlens r) editorB
floatingMapEditor (MkFloatingChangeLens (ReadFloatInit finit) rlens) editorB =
    MkEditor $ \refA -> do
        r <- viewRunResource refA $ \arefA -> finit $ refRead arefA
        let MkEditor editorA = mapEditor (rlens r) editorB
        editorA refA
