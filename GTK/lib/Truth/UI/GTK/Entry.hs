{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.Entry(entryTypeKnowledge) where
{
    import Prelude;
    import Graphics.UI.Gtk;
    import Control.Monad.IsStateIO;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;
    import Truth.UI.GTK.Useful;


    textEntryView :: GView (WholeEdit String);
    textEntryView = MkView $ \(MkObject object) _setSelect -> do
    {
        widget <- entryNew;
        initial <- object $ \muted -> unReadable pureFromReader $ mutableRead muted;
        set widget [entryText := initial];
        clickConnection <- on widget editableChanged $ object $ \muted -> do
        {
            s <- liftIO $ get widget entryText;
            _ <- mutableEdit muted $ getReplaceEdits s;
            return ();
        };

        let
        {
            vrWidget = toWidget widget;
            vrUpdate :: forall m. IsStateIO m => MutableRead m (WholeReader String) -> [WholeEdit String] -> m ();
            vrUpdate _ edits = liftIO $ do
            {
                newstate <- fromReadFunctionM (applyEdits edits) $ get widget entryText;
                withSignalBlocked clickConnection $ set widget [entryText := newstate];
                return ();
            };
            vrFirstAspectGetter = return Nothing;
        };
        return MkViewResult{..};
    };

    -- orphan
    instance DependentHasView Widget (WholeEdit String);
    -- orphan
    instance HasView Widget (WholeEdit String) where
    {
        theView = textEntryView;
    };

    entryTypeKnowledge :: TypeKnowledge;
    entryTypeKnowledge = namedKnowledge "text entry" $(generateTypeKnowledge [d|
        instance DependentHasView Widget (WholeEdit String);
    |]);
}
