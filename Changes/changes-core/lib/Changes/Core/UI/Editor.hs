module Changes.Core.UI.Editor where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model
import Changes.Core.UI.View.CreateView
import Changes.Core.UI.View.View

type Editing :: Type -> Type -> Type
data Editing update r = MkEditing
    { editingUpdate :: NonEmpty update -> EditContext -> View ()
    , editingTask :: Task ()
    , editingDo :: Task () -> CreateView r
    }

instance Functor (Editing update) where
    fmap ab (MkEditing eu et ed) = MkEditing eu et $ \ut -> fmap ab $ ed ut

instance Applicative (Editing update) where
    pure a = let
        editingUpdate _ _ = return ()
        editingTask = mempty
        editingDo _ = return a
        in MkEditing {..}
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
        in MkEditing {..}

{-
mapEditing :: forall updateA updateB r. ChangeLens updateA updateB -> Editing updateB r -> Editing updateA r
mapEditing l (MkEditing euB et edB) = let
    euA :: Reference (UpdateEdit updateA) -> NonEmpty updateA -> EditContext -> View ()
    euA refA updatesA ec = do
        updatesB <- mapM (\uA -> l (viewUpdate uA) uA) updatesA
        euB refA updatesB ec
    edA :: Reference (UpdateEdit updateA) -> Task () -> CreateView r
    edA refA t = edB (mapReference l refA) t
    in MkEditing euA et edA
-}
type Editor :: Type -> Type -> Type
newtype Editor update r =
    MkEditor (Reference (UpdateEdit update) -> CreateView (Editing update r))

instance Functor (Editor update) where
    fmap ab (MkEditor ed) = MkEditor $ fmap (fmap (fmap ab)) ed

instance Applicative (Editor update) where
    pure a = MkEditor $ pure $ pure $ pure a
    (MkEditor ed1) <*> (MkEditor ed2) = MkEditor $ liftA2 (liftA2 (<*>)) ed1 ed2

runEditor :: Model update -> Editor update r -> CreateView r
runEditor model (MkEditor ed) = do
    e <- cvBindModelUpdates model (\_ -> True) (ed $ modelReference model) editingTask editingUpdate
    editingDo e $ modelUpdatesTask model

execEditor :: CreateView (Editor update r) -> Editor update r
execEditor cv =
    MkEditor $ \ref -> do
        MkEditor ed <- cv
        ed ref
