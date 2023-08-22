module Changes.World.GNOME.GTK.Widget.ListBox
    ( createListBox
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gtk
import Shapes

newtype ListViewState =
    MkListViewState [GViewState 'Unlocked]

insertListViewState :: Monad m => SequencePoint -> GViewState 'Unlocked -> StateT ListViewState m ()
insertListViewState i vs = do
    MkListViewState s <- Shapes.get
    let (l, r) = splitAt (fromIntegral i) s
    put $ MkListViewState $ l <> (vs : r)

removeListViewState :: Monad m => SequencePoint -> StateT ListViewState m (GViewState 'Unlocked)
removeListViewState i = do
    MkListViewState s <- Shapes.get
    let (l, r) = splitAt (fromIntegral i) s
    put $ MkListViewState $ l <> drop 1 r
    return $ fromJust $ listToMaybe r

removeAllListViewStates :: Monad m => StateT ListViewState m [GViewState 'Unlocked]
removeAllListViewStates = do
    MkListViewState s <- Shapes.get
    put $ MkListViewState []
    return s

createListBox ::
       forall update. (IsUpdate update, FullSubjectReader (UpdateReader update), ApplicableEdit (UpdateEdit update))
    => (Model update -> GView 'Unlocked Widget)
    -> Model (OrderedListUpdate update)
    -> GView 'Unlocked Widget
createListBox mkWidget model = do
    (listBox, widget) <-
        gvRunLocked $ gvNewWidget ListBox [#selectionMode := SelectionModeSingle, #activateOnSingleClick := True]
    let
        insertWidget :: SequencePoint -> GView 'Unlocked (GViewState 'Unlocked)
        insertWidget i = do
            ((), vs) <-
                gvGetState $ do
                    imodel <-
                        gvLiftView $
                        viewFloatMapModel
                            (changeLensToFloating (mustExistOneChangeLens "GTK list box") . orderedListItemLens i)
                            model
                    iwidget <- mkWidget imodel
                    gvRunLocked $ #insert listBox iwidget (fromIntegral i)
                    return ()
            return vs
        initVS :: GView 'Unlocked (ListViewState, ())
        initVS = do
            n <- gvLiftView $ viewRunResource model $ \am -> aModelRead am ListReadLength
            vss <- for [0 .. pred n] insertWidget
            return (MkListViewState vss, ())
        recvVS :: () -> [OrderedListUpdate update] -> StateT ListViewState (GView 'Unlocked) ()
        recvVS () updates =
            for_ updates $ \case
                OrderedListUpdateItem i j _
                    | i == j -> return ()
                OrderedListUpdateItem i j _ -> do
                    mrow <- lift $ gvRunLocked $ #getRowAtIndex listBox (fromIntegral i)
                    case mrow of
                        Nothing -> return ()
                        Just row -> do
                            lift $
                                gvRunLocked $ do
                                    #remove listBox row
                                    #insert listBox row (fromIntegral j)
                            vs <- removeListViewState i
                            insertListViewState j vs
                OrderedListUpdateDelete i -> do
                    mrow <- lift $ gvRunLocked $ #getRowAtIndex listBox (fromIntegral i)
                    case mrow of
                        Nothing -> return ()
                        Just row -> lift $ gvRunLocked $ #remove listBox row
                    vs <- removeListViewState i
                    lift $ do gvCloseState vs
                OrderedListUpdateInsert i _ -> do
                    vs <- lift $ insertWidget i
                    insertListViewState i vs
                OrderedListUpdateClear -> do
                    lift $
                        gvRunLocked $ do
                            ws <- #getChildren listBox
                            for_ ws $ #remove listBox
                    vss <- removeAllListViewStates
                    lift $ do gvCloseState $ mconcat vss
        toVS :: ListViewState -> IO (GViewState 'Unlocked)
        toVS (MkListViewState vs) = return $ mconcat vs
    gvDynamic model initVS toVS mempty recvVS
    return widget
