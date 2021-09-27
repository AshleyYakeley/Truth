module Changes.UI.GTK.ListBox
    ( createListBox
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk
import Shapes

newtype ListViewState =
    MkListViewState [ViewState]

instance DynamicViewState ListViewState where
    dynamicViewStates (MkListViewState vss) = vss

insertListViewState :: SequencePoint -> ViewState -> StateT ListViewState View ()
insertListViewState i vs = do
    MkListViewState s <- Shapes.get
    let (l, r) = splitAt (fromIntegral i) s
    put $ MkListViewState $ l <> (vs : r)

removeListViewState :: SequencePoint -> StateT ListViewState View ViewState
removeListViewState i = do
    MkListViewState s <- Shapes.get
    let (l, r) = splitAt (fromIntegral i) s
    put $ MkListViewState $ l <> drop 1 r
    return $ fromJust $ listToMaybe r

removeAllListViewStates :: StateT ListViewState View [ViewState]
removeAllListViewStates = do
    MkListViewState s <- Shapes.get
    put $ MkListViewState []
    return s

createListBox ::
       forall update. (IsUpdate update, FullSubjectReader (UpdateReader update), ApplicableEdit (UpdateEdit update))
    => (Model update -> CreateView Widget)
    -> Model (OrderedListUpdate update)
    -> CreateView Widget
createListBox mkElement model = do
    listBox <- cvNew ListBox [#selectionMode := SelectionModeSingle, #activateOnSingleClick := True]
    let
        insertElement :: SequencePoint -> View ViewState
        insertElement i = do
            fmap snd $
                getInnerLifeState $ do
                    imodel <-
                        cvFloatMapModel
                            (changeLensToFloating (mustExistOneChangeLens "GTK list box") . orderedListItemLens i)
                            model
                    iwidget <- mkElement imodel
                    #insert listBox iwidget (fromIntegral i)
                    return ()
        initVS :: CreateView (ListViewState, ())
        initVS = do
            n <- viewRunResource model $ \am -> aModelRead am ListReadLength
            vss <- liftToLifeCycle $ for [0 .. pred n] insertElement
            return (MkListViewState vss, ())
        recvVS :: () -> [OrderedListUpdate update] -> StateT ListViewState View ()
        recvVS () updates =
            for_ updates $ \case
                OrderedListUpdateItem i j _
                    | i == j -> return ()
                OrderedListUpdateItem i j _ -> do
                    mrow <- #getRowAtIndex listBox (fromIntegral i)
                    case mrow of
                        Nothing -> return ()
                        Just row -> do
                            #remove listBox row
                            #insert listBox row (fromIntegral j)
                            vs <- removeListViewState i
                            insertListViewState j vs
                OrderedListUpdateDelete i -> do
                    mrow <- #getRowAtIndex listBox (fromIntegral i)
                    case mrow of
                        Nothing -> return ()
                        Just row -> #remove listBox row
                    vs <- removeListViewState i
                    liftIO $ closeLifeState vs
                OrderedListUpdateInsert i _ -> do
                    vs <- lift $ insertElement i
                    insertListViewState i vs
                OrderedListUpdateClear -> do
                    ws <- #getChildren listBox
                    for_ ws $ #remove listBox
                    vss <- removeAllListViewStates
                    liftIO $ for_ vss closeLifeState
    cvDynamic model initVS mempty recvVS
    toWidget listBox
