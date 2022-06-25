module Changes.UI.GTK.ListBox
    ( createListBox
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk
import Shapes

newtype ListViewState =
    MkListViewState [GViewState 'Locked]

insertListViewState :: Monad m => SequencePoint -> GViewState 'Locked -> StateT ListViewState m ()
insertListViewState i vs = do
    MkListViewState s <- Shapes.get
    let (l, r) = splitAt (fromIntegral i) s
    put $ MkListViewState $ l <> (vs : r)

removeListViewState :: Monad m => SequencePoint -> StateT ListViewState m (GViewState 'Locked)
removeListViewState i = do
    MkListViewState s <- Shapes.get
    let (l, r) = splitAt (fromIntegral i) s
    put $ MkListViewState $ l <> drop 1 r
    return $ fromJust $ listToMaybe r

removeAllListViewStates :: Monad m => StateT ListViewState m [GViewState 'Locked]
removeAllListViewStates = do
    MkListViewState s <- Shapes.get
    put $ MkListViewState []
    return s

createListBox ::
       forall update. (IsUpdate update, FullSubjectReader (UpdateReader update), ApplicableEdit (UpdateEdit update))
    => (Model update -> GView 'Locked Widget)
    -> Model (OrderedListUpdate update)
    -> GView 'Locked Widget
createListBox mkElement model = do
    listBox <- gvNew ListBox [#selectionMode := SelectionModeSingle, #activateOnSingleClick := True]
    runLockedState <- gvRunLockedState
    let
        insertElement :: SequencePoint -> GView 'Locked (GViewState 'Locked)
        insertElement i = do
            ((), vs) <-
                gvGetState $ do
                    imodel <-
                        gvLiftViewNoUI $
                        viewFloatMapModel
                            (changeLensToFloating (mustExistOneChangeLens "GTK list box") . orderedListItemLens i)
                            model
                    iwidget <- mkElement imodel
                    #insert listBox iwidget (fromIntegral i)
                    return ()
            return vs
        initVS :: GView 'Unlocked (ListViewState, ())
        initVS =
            gvRunLocked $ do
                n <- gvLiftViewNoUI $ viewRunResource model $ \am -> aModelRead am ListReadLength
                vss <- for [0 .. pred n] insertElement
                return (MkListViewState vss, ())
        recvVS :: () -> [OrderedListUpdate update] -> StateT ListViewState (GView 'Unlocked) ()
        recvVS () updates =
            for_ updates $ \case
                OrderedListUpdateItem i j _
                    | i == j -> return ()
                OrderedListUpdateItem i j _ ->
                    hoist gvRunLocked $ do
                        mrow <- #getRowAtIndex listBox (fromIntegral i)
                        case mrow of
                            Nothing -> return ()
                            Just row -> do
                                #remove listBox row
                                #insert listBox row (fromIntegral j)
                                vs <- removeListViewState i
                                insertListViewState j vs
                OrderedListUpdateDelete i ->
                    hoist gvRunLocked $ do
                        mrow <- #getRowAtIndex listBox (fromIntegral i)
                        case mrow of
                            Nothing -> return ()
                            Just row -> #remove listBox row
                        vs <- removeListViewState i
                        lift $ do gvCloseState vs
                OrderedListUpdateInsert i _ ->
                    hoist gvRunLocked $ do
                        vs <- lift $ insertElement i
                        insertListViewState i vs
                OrderedListUpdateClear ->
                    hoist gvRunLocked $ do
                        ws <- #getChildren listBox
                        for_ ws $ #remove listBox
                        vss <- removeAllListViewStates
                        lift $ do gvCloseState $ mconcat vss
        toVS :: ListViewState -> IO (GViewState 'Unlocked)
        toVS (MkListViewState vs) = return $ runLockedState $ mconcat vs
    gvRunUnlocked $ gvDynamic model initVS toVS mempty recvVS
    toWidget listBox
