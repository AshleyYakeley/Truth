{-# OPTIONS -fno-warn-redundant-constraints #-}

module Truth.Core
    ( module Truth.Core.Sequence
    , module Truth.Core.Read
    , module Truth.Core.Edit
    , module Truth.Core.Types
    , module Truth.Core.Object
    , module Truth.Core.UI
    , module Truth.Core
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Read
import Truth.Core.Sequence
import Truth.Core.Types
import Truth.Core.UI

type ReasonCodec = Codec' (Result String)

data Reference edit = forall m. MonadCombineIO m =>
                                MkReference
    { refRun :: UnliftIO m
    , refRead :: MutableRead m (EditReader edit)
    , refEdit :: [edit] -> m (Maybe (m ()))
    }

data AnEditFunction t edita editb = MkAnEditFunction
    { efGet :: forall m. MonadIO m =>
                             MutableRead m (EditReader edita) -> MutableRead (t m) (EditReader editb)
    , efUpdate :: forall m. MonadIO m =>
                                edita -> MutableRead m (EditReader edita) -> t m [editb]
    }

type Unlift (t :: (* -> *) -> * -> *) = forall (m :: * -> *) (r :: *). t m r -> m r

identityUnlift :: Unlift IdentityT
identityUnlift = runIdentityT

composeUnlift :: Unlift ta -> Unlift tb -> Unlift (ComposeT ta tb)
composeUnlift ua ub (MkComposeT tatbma) = ub $ ua tatbma

data CloseUnlift f edita editb =
    forall t. MonadTransUnlift t =>
              MkCloseUnlift (Unlift t)
                            (f t edita editb)

class UnliftCategory (f :: ((* -> *) -> (* -> *)) -> * -> * -> *) where
    type UnliftCategoryConstraint f (a :: *) :: Constraint
    ucId ::
           forall a. UnliftCategoryConstraint f a
        => f IdentityT a a
    ucCompose ::
           forall tab tbc a b c.
           ( UnliftCategoryConstraint f a
           , UnliftCategoryConstraint f b
           , UnliftCategoryConstraint f c
           , MonadTransConstraint MonadIO tab
           , MonadTransConstraint MonadIO tbc
           )
        => f tbc b c
        -> f tab a b
        -> f (ComposeT tbc tab) a c

instance UnliftCategory f => ConstrainedCategory (CloseUnlift f) where
    type CategoryConstraint (CloseUnlift f) a = UnliftCategoryConstraint f a
    cid = MkCloseUnlift identityUnlift ucId
    (MkCloseUnlift unliftBC fBC) <.> (MkCloseUnlift unliftAB fAB) =
        MkCloseUnlift (composeUnlift unliftBC unliftAB) (ucCompose fBC fAB)

type EditFunction' = CloseUnlift AnEditFunction

instance UnliftCategory AnEditFunction where
    type UnliftCategoryConstraint AnEditFunction edit = ()
    ucId = let
        efGet = remonadMutableRead IdentityT
        efUpdate edit _ = IdentityT $ return [edit]
        in MkAnEditFunction {..}
    ucCompose ::
           forall tab tbc edita editb editc. (MonadTransConstraint MonadIO tab, MonadTransConstraint MonadIO tbc)
        => AnEditFunction tbc editb editc
        -> AnEditFunction tab edita editb
        -> AnEditFunction (ComposeT tbc tab) edita editc
    ucCompose (MkAnEditFunction gBC uBC) (MkAnEditFunction gAB uAB) = let
        gAC :: forall m. MonadIO m
            => MutableRead m (EditReader edita)
            -> MutableRead (ComposeT tbc tab m) (EditReader editc)
        gAC mra =
            case hasTransConstraint @MonadIO @tab @m of
                Dict ->
                    case hasTransConstraint @MonadIO @tbc @(tab m) of
                        Dict -> remonadMutableRead MkComposeT $ gBC @(tab m) $ gAB @m mra
        uAC :: forall m. MonadIO m
            => edita
            -> MutableRead m (EditReader edita)
            -> ComposeT tbc tab m [editc]
        uAC editA mrA =
            case hasTransConstraint @MonadIO @tab @m of
                Dict ->
                    case hasTransConstraint @MonadIO @tbc @(tab m) of
                        Dict ->
                            MkComposeT $ do
                                editbs <- lift $ uAB editA mrA
                                editcss <- for editbs $ \editb -> uBC editb $ gAB mrA
                                return $ mconcat editcss
        in MkAnEditFunction gAC uAC

instance Category EditFunction' where
    id = cid
    (.) = (<.>)

efUpdates ::
       (MonadIO m, Monad (t m))
    => AnEditFunction t edita editb
    -> [edita]
    -> MutableRead m (EditReader edita)
    -> t m [editb]
efUpdates _ [] _ = return []
efUpdates sef (ea:eas) mr = do
    eb <- efUpdate sef ea mr
    ebs <- efUpdates sef eas mr
    return $ eb ++ ebs

data AnEditLens t edita editb = MkAnEditLens
    { elFunction :: AnEditFunction t edita editb
    , elPutEdit :: forall m. MonadIO m =>
                                 editb -> MutableRead m (EditReader edita) -> t m (Maybe [edita])
    }

type EditLens' = CloseUnlift AnEditLens

instance UnliftCategory AnEditLens where
    type UnliftCategoryConstraint AnEditLens edit = Edit edit
    ucId = let
        pe :: forall m edit. MonadIO m
           => edit
           -> MutableRead m (EditReader edit)
           -> IdentityT m (Maybe [edit])
        pe edit _ = return $ Just [edit]
        in MkAnEditLens ucId pe
    ucCompose ::
           forall tab tbc edita editb editc.
           (MonadTransConstraint MonadIO tab, MonadTransConstraint MonadIO tbc, Edit edita, Edit editb, Edit editc)
        => AnEditLens tbc editb editc
        -> AnEditLens tab edita editb
        -> AnEditLens (ComposeT tbc tab) edita editc
    ucCompose (MkAnEditLens efBC peBC) lensAB@(MkAnEditLens efAB _) = let
        peAC ::
               forall m. MonadIO m
            => editc
            -> MutableRead m (EditReader edita)
            -> ComposeT tbc tab m (Maybe [edita])
        peAC ec mra =
            case hasTransConstraint @MonadIO @tab @m of
                Dict ->
                    case hasTransConstraint @MonadIO @tbc @(tab m) of
                        Dict ->
                            getCompose $ do
                                ebs <- Compose $ MkComposeT $ peBC ec $ efGet efAB mra
                                Compose $ lift2ComposeT $ elPutEdits lensAB ebs mra
        efAC = ucCompose efBC efAB
        in MkAnEditLens efAC peAC

elPutEdits ::
       (MonadIO m, Monad (t m), Edit edita)
    => AnEditLens t edita editb
    -> [editb]
    -> MutableRead m (EditReader edita)
    -> t m (Maybe [edita])
elPutEdits _ [] _ = getCompose $ return []
elPutEdits lens (e:ee) mr =
    getCompose $ do
        ea <- Compose $ elPutEdit lens e mr
        eea <- Compose $ elPutEdits lens ee $ mapMutableRead (applyEdits ea) $ mapMutableRead (applyEdits ea) mr
        return $ ea ++ eea

mapReference ::
       forall edita editb. Edit edita
    => EditLens' edita editb
    -> Reference edita
    -> Reference editb
mapReference (MkCloseUnlift (lensRun :: Unlift tl) lens@MkAnEditLens {..}) (MkReference (refRunA :: UnliftIO mr) refReadA refEditA)
    | Dict <- hasTransConstraint @MonadIO @tl @mr = let
        MkAnEditFunction {..} = elFunction
        refRunB :: UnliftIO (tl mr)
        refRunB = refRunA . lensRun
        refReadB :: MutableRead (tl mr) (EditReader editb)
        refReadB = efGet refReadA
        refEditB :: [editb] -> tl mr (Maybe (tl mr ()))
        refEditB editbs = do
            meditas <- elPutEdits lens editbs refReadA
            case meditas of
                Nothing -> return Nothing
                Just editas -> do
                    mmu <- lift $ refEditA editas
                    case mmu of
                        Nothing -> return Nothing
                        Just mu -> return $ Just $ lift mu
        in MkReference @editb @(tl mr) refRunB refReadB refEditB

pairReferences :: forall edita editb. Reference edita -> Reference editb -> Reference (PairEdit edita editb)
pairReferences (MkReference (runA :: UnliftIO ma) readA editA) (MkReference (runB :: UnliftIO mb) readB editB) =
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
            in MkReference runAB readAB editAB
