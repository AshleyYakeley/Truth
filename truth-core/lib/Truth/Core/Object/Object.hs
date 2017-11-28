module Truth.Core.Object.Object where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.MutableEdit
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.Pair
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole

data Object' edit = forall m. MonadStackIO m =>
                              MkObject'
    { objRun :: UnliftIO m
    , objRead :: MutableRead m (EditReader edit)
    , objEdit :: [edit] -> m (Maybe (m ()))
    }

mapObject' ::
       forall edita editb. Edit edita
    => EditLens' edita editb
    -> Object' edita
    -> Object' editb
mapObject' (MkCloseUnlift (lensRun :: Unlift tl) lens@MkAnEditLens {..}) (MkObject' (objRunA :: UnliftIO mr) objReadA objEditA)
    | Dict <- hasTransConstraint @MonadIO @tl @mr = let
        MkAnEditFunction {..} = elFunction
        objRunB :: UnliftIO (tl mr)
        objRunB = objRunA . lensRun
        objReadB :: MutableRead (tl mr) (EditReader editb)
        objReadB = efGet objReadA
        objEditB :: [editb] -> tl mr (Maybe (tl mr ()))
        objEditB editbs = do
            meditas <- elPutEdits lens editbs objReadA
            case meditas of
                Nothing -> return Nothing
                Just editas -> do
                    mmu <- lift $ objEditA editas
                    case mmu of
                        Nothing -> return Nothing
                        Just mu -> return $ Just $ lift mu
        in MkObject' @editb @(tl mr) objRunB objReadB objEditB

pairObjects' :: forall edita editb. Object' edita -> Object' editb -> Object' (PairEdit edita editb)
pairObjects' (MkObject' (runA :: UnliftIO ma) readA editA) (MkObject' (runB :: UnliftIO mb) readB editB) =
    case isCombineMonadIO @ma @mb of
        Dict -> let
            runAB :: UnliftIO (CombineMonadIO ma mb)
            runAB = combineUnliftIOs runA runB
            readAB :: MutableRead (CombineMonadIO ma mb) (PairEditReader edita editb)
            readAB (MkTupleEditReader EditFirst r) = combineLiftFst @ma @mb $ readA r
            readAB (MkTupleEditReader EditSecond r) = combineLiftSnd @ma @mb $ readB r
            editAB :: [PairEdit edita editb] -> CombineMonadIO ma mb (Maybe (CombineMonadIO ma mb ()))
            editAB edits = let
                (eas, ebs) = partitionPairEdits edits
                in liftA2
                       (liftA2 $ \mau mbu -> (>>) (combineLiftFst @ma @mb mau) (combineLiftSnd @ma @mb mbu))
                       (combineLiftFst @ma @mb $ editA eas)
                       (combineLiftSnd @ma @mb $ editB ebs)
            in MkObject' runAB readAB editAB

newtype Object edit = MkObject
    { runObject :: forall r. (forall m. IsStateIO m =>
                                            MutableEdit m edit -> m r) -> IO r
    }

nonlockingObject :: MutableEdit IO edit -> Object edit
nonlockingObject muted = MkObject $ \call -> call muted

noneObject :: Object (NoEdit (NoReader t))
noneObject = nonlockingObject noneMutableEdit

mvarObject :: forall a. MVar a -> (a -> Bool) -> Object (WholeEdit a)
mvarObject var allowed =
    MkObject $ \call ->
        mvarStateAccess var $ let
            muted :: MutableEdit (StateT a IO) (WholeEdit a)
            muted = let
                mutableRead :: MutableRead (StateT a IO) (WholeReader a)
                mutableRead ReadWhole = get
                mutableEdit edits = do
                    na <- fromReadFunctionM (applyEdits edits) get
                    return $
                        if allowed na
                            then Just $ put na
                            else Nothing
                in MkMutableEdit {..}
            in call muted

freeIOObject :: forall a. a -> (a -> Bool) -> IO (Object (WholeEdit a))
freeIOObject firsta allowed = do
    var <- newMVar firsta
    return $ mvarObject var allowed

mapObject ::
       forall edita editb. (Edit edita)
    => GeneralLens edita editb
    -> Object edita
    -> Object editb
mapObject (MkCloseState lens) objectA =
    MkObject $ \callB ->
        runObject objectA $ \mutedA -> do
            oldstate <- editAccess (editLensFunction lens) get
            (r, _newstate) <- runStateT (callB $ mapMutableEdit lens mutedA) oldstate -- ignore the new lens state: all these lens changes will be replayed by the update
            return r

convertObject :: (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb) => Object edita -> Object editb
convertObject = mapObject convertGeneralLens

-- | Combines all the edits made in each call to the object.
cacheObject :: Eq t => Object (WholeEdit t) -> Object (WholeEdit t)
cacheObject (MkObject obj) =
    MkObject $ \call ->
        obj $ \muted -> do
            oldval <- mutableRead muted ReadWhole
            let
                muted' =
                    MkMutableEdit
                    {mutableRead = \ReadWhole -> get, mutableEdit = singleAlwaysMutableEdit $ \(MkWholeEdit t) -> put t}
            (r, newval) <- runStateT (call muted') oldval
            if oldval == newval
                then return ()
                else do
                    maction <- mutableEdit muted $ [MkWholeEdit newval]
                    case maction of
                        Just action -> action
                        Nothing -> liftIO $ fail "disallowed cached edit"
            return r

pairObject :: Object ea -> Object eb -> Object (PairEdit ea eb)
pairObject (MkObject objectA) (MkObject objectB) =
    MkObject $ \call ->
        objectA $ \mutedA ->
            mkStateIO $ \oldsA ->
                objectB $ \mutedB ->
                    mkStateIO $ \oldsB ->
                        fmap swap3 $
                        runStateT
                            (let
                                 mutedA' = remonadMutableEdit (stateFst . fromStateIO) mutedA
                                 mutedB' = remonadMutableEdit (stateSnd . fromStateIO) mutedB
                                 mutedAB = pairMutableEdit mutedA' mutedB'
                                 in call mutedAB)
                            (oldsA, oldsB)
