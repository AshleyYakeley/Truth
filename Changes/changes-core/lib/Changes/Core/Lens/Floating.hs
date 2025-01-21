module Changes.Core.Lens.Floating
    ( FloatInit (..)
    , runFloatInit
    , mapFloatInit
    , mapFFloatInit
    , ExpFloatingChangeLens (..)
    , composeExpFloatingChangeLens
    , LinearFloatingChangeLens
    , changeLensToExpFloating
    , FloatingChangeLens (..)
    , expToFloatingChangeLens
    , runFloatingChangeLens
    , changeLensToFloating
    , floatingToMaybeChangeLens
    , floatingToDiscardingChangeLens
    , floatLift
    )
where

import Changes.Core.Edit.Update
import Changes.Core.Import
import Changes.Core.Lens.Lens
import Changes.Core.Read

data FloatInit reader r where
    ReadFloatInit :: forall reader r. (forall m. MonadIO m => Readable m reader -> m r) -> FloatInit reader r
    NoFloatInit :: forall reader r. r -> FloatInit reader r

runFloatInit :: forall reader r. FloatInit reader r -> forall m. MonadIO m => Readable m reader -> m r
runFloatInit (ReadFloatInit finit) = finit
runFloatInit (NoFloatInit r) = \_ -> return r

mapFloatInit :: ReadFunction readerB readerA -> FloatInit readerA r -> FloatInit readerB r
mapFloatInit _ (NoFloatInit r) = NoFloatInit r
mapFloatInit rf (ReadFloatInit i) = ReadFloatInit $ \mr -> i $ rf mr

mapFFloatInit :: MonadInner f => ReadFunctionF f readerB readerA -> FloatInit readerA r -> FloatInit readerB (f r)
mapFFloatInit rf finit = ReadFloatInit $ \mr -> unComposeInner $ runFloatInit finit $ rf mr

type ExpFloatingChangeLens :: Linearity -> Type -> Type -> Type -> Type
data ExpFloatingChangeLens lin r updateA updateB
    = MkExpFloatingChangeLens
        (FloatInit (UpdateReader updateA) r)
        (r -> GenChangeLens lin updateA updateB)

composeExpFloatingChangeLens ::
    forall lin rAB rBC updateA updateB updateC.
    IsLinearity lin =>
    ExpFloatingChangeLens lin rBC updateB updateC ->
    ExpFloatingChangeLens lin rAB updateA updateB ->
    ExpFloatingChangeLens lin (rAB, rBC) updateA updateC
composeExpFloatingChangeLens (MkExpFloatingChangeLens initBC rlensBC) (MkExpFloatingChangeLens initAB rlensAB) = let
    initAC :: FloatInit (UpdateReader updateA) (rAB, rBC)
    initAC =
        case (initAB, initBC) of
            (NoFloatInit rab, NoFloatInit rbc) -> NoFloatInit (rab, rbc)
            _ ->
                ReadFloatInit $ \rda -> do
                    rab <- runFloatInit initAB rda
                    rbc <- runFloatInit initBC $ clRead (rlensAB rab) rda
                    return (rab, rbc)
    rlensAC :: (rAB, rBC) -> GenChangeLens lin updateA updateC
    rlensAC (rab, rbc) = rlensBC rbc . rlensAB rab
    in MkExpFloatingChangeLens initAC rlensAC

type LinearFloatingChangeLens = ExpFloatingChangeLens 'Linear

changeLensToExpFloating :: GenChangeLens lin updateA updateB -> ExpFloatingChangeLens lin () updateA updateB
changeLensToExpFloating lens = MkExpFloatingChangeLens (NoFloatInit ()) $ \_ -> lens

type FloatingChangeLens :: Type -> Type -> Type
data FloatingChangeLens updateA updateB = forall r. MkFloatingChangeLens
    { fclInit :: FloatInit (UpdateReader updateA) r
    , fclLens :: r -> ChangeLens updateA updateB
    }

expToFloatingChangeLens ::
    IsLinearity lin => ExpFloatingChangeLens lin r updateA updateB -> FloatingChangeLens updateA updateB
expToFloatingChangeLens (MkExpFloatingChangeLens finit rlens) =
    MkFloatingChangeLens finit $ \r -> linearToChangeLens $ rlens r

changeLensToFloating :: ChangeLens updateA updateB -> FloatingChangeLens updateA updateB
changeLensToFloating lens = MkFloatingChangeLens (NoFloatInit ()) $ \_ -> lens

floatingToMaybeChangeLens :: FloatingChangeLens updateA updateB -> Maybe (ChangeLens updateA updateB)
floatingToMaybeChangeLens (MkFloatingChangeLens (NoFloatInit r) f) = Just $ f r
floatingToMaybeChangeLens _ = Nothing

floatingToDiscardingChangeLens ::
    forall updateA updateB. FloatingChangeLens updateA updateB -> ChangeLens updateA updateB
floatingToDiscardingChangeLens (MkFloatingChangeLens (NoFloatInit r) rlens) = rlens r
floatingToDiscardingChangeLens (MkFloatingChangeLens (ReadFloatInit finit) rlens) = let
    g :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    g mr rt = do
        r <- finit mr
        clRead (rlens r) mr rt
    u ::
        forall m.
        MonadIO m =>
        updateA ->
        Readable m (UpdateReader updateA) ->
        m [updateB]
    u upd mr = do
        r <- finit mr
        clUpdate (rlens r) upd mr
    pe ::
        forall m.
        MonadIO m =>
        [UpdateEdit updateB] ->
        Readable m (UpdateReader updateA) ->
        m (Maybe [UpdateEdit updateA])
    pe edits mr = do
        r <- finit mr
        clPutEdits (rlens r) edits mr
    in MkChangeLens g u pe

runFloatingChangeLens ::
    forall updateA updateB. IO (FloatingChangeLens updateA updateB) -> FloatingChangeLens updateA updateB
runFloatingChangeLens iol = let
    fclInit :: FloatInit (UpdateReader updateA) (ChangeLens updateA updateB)
    fclInit =
        ReadFloatInit $ \mr -> do
            MkFloatingChangeLens finit rlens <- liftIO iol
            r <- runFloatInit finit mr
            return $ rlens r
    in MkFloatingChangeLens fclInit id

floatLift ::
    ReadFunction (UpdateReader updateC) (UpdateReader updateA) ->
    (ChangeLens updateA updateB -> ChangeLens updateC updateD) ->
    FloatingChangeLens updateA updateB ->
    FloatingChangeLens updateC updateD
floatLift rf mappl (MkFloatingChangeLens finit rlens) =
    MkFloatingChangeLens (mapFloatInit rf finit) $ \r -> mappl $ rlens r

instance Category FloatingChangeLens where
    id :: forall update. FloatingChangeLens update update
    id = changeLensToFloating id
    (.) ::
        forall updateA updateB updateC.
        FloatingChangeLens updateB updateC ->
        FloatingChangeLens updateA updateB ->
        FloatingChangeLens updateA updateC
    lensBC . lensAB
        | Just plensBC <- floatingToMaybeChangeLens lensBC
        , Just plensAB <- floatingToMaybeChangeLens lensAB =
            changeLensToFloating $ plensBC . plensAB
    MkFloatingChangeLens initBC rlensBC . MkFloatingChangeLens initAB rlensAB = let
        initAC =
            ReadFloatInit $ \mr -> do
                r1 <- runFloatInit initAB mr
                r2 <- runFloatInit initBC $ clRead (rlensAB r1) mr
                return (r1, r2)
        rlensAC (r1, r2) = rlensBC r2 . rlensAB r1
        in MkFloatingChangeLens initAC rlensAC
