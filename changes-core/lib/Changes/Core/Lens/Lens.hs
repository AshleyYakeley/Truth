module Changes.Core.Lens.Lens where

import Changes.Core.Edit.Edit
import Changes.Core.Edit.FullEdit
import Changes.Core.Edit.Update
import Changes.Core.Import
import Changes.Core.Read

data Linearity
    = Linear
    | NonLinear

type NullReader :: Type -> Type
data NullReader t

nullReadable :: Readable m NullReader
nullReadable = \case {}

type IsLinearity :: Linearity -> Constraint
class IsLinearity lin where
    type NL lin (r :: Type -> Type) :: Type -> Type
    linear :: NL lin rd t -> rd t
    nlLiftReadFunction :: forall ra rb. ReadFunction ra rb -> ReadFunction (NL lin ra) (NL lin rb)

instance IsLinearity 'Linear where
    type NL 'Linear r = NullReader
    linear = nullReadable
    nlLiftReadFunction _ rd = rd

instance IsLinearity 'NonLinear where
    type NL 'NonLinear r = r
    linear = id
    nlLiftReadFunction rf = rf

{-
Cleaner version:

data GenChangeLens lin updateA updateB = MkChangeLens
    { clRead :: Readable (ReadM (UpdateReader updateA)) (UpdateReader updateB)
    , clUpdate :: updateA -> ReadM (UpdateReader updateA) [updateB]
    -- ^ the Readable argument will reflect the new value
    , clPutEdits :: [UpdateEdit updateB] -> ReadM (NL lin (UpdateReader updateA)) (Maybe [UpdateEdit updateA])
    }
-}
type GenChangeLens :: Linearity -> Type -> Type -> Type
data GenChangeLens lin updateA updateB = MkChangeLens
    { clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    , clUpdate :: forall m. MonadIO m => updateA -> Readable m (UpdateReader updateA) -> m [updateB]
    -- ^ the Readable argument will reflect the new value
    , clPutEdits :: forall m.
                        MonadIO m =>
                                [UpdateEdit updateB] -> Readable m (NL lin (UpdateReader updateA)) -> m (Maybe [UpdateEdit updateA])
    }

type ChangeLens = GenChangeLens 'NonLinear

type LinearChangeLens = GenChangeLens 'Linear

readableToLinear ::
       forall lin rd m. IsLinearity lin
    => Readable m rd
    -> Readable m (NL lin rd)
readableToLinear rd t = rd $ linear @lin t

linearToChangeLens ::
       forall lin updateA updateB. IsLinearity lin
    => GenChangeLens lin updateA updateB
    -> ChangeLens updateA updateB
linearToChangeLens (MkChangeLens r u pe) = MkChangeLens r u $ \edits rd -> pe edits $ readableToLinear @lin rd

instance IsLinearity lin => Category (GenChangeLens lin) where
    id :: forall update. GenChangeLens lin update update
    id = let
        clRead :: ReadFunction (UpdateReader update) (UpdateReader update)
        clRead mr = mr
        clUpdate ::
               forall m. MonadIO m
            => update
            -> Readable m (UpdateReader update)
            -> m [update]
        clUpdate update _ = return [update]
        clPutEdits ::
               forall m. MonadIO m
            => [UpdateEdit update]
            -> Readable m (NL lin (UpdateReader update))
            -> m (Maybe [UpdateEdit update])
        clPutEdits edits _ = return $ Just edits
        in MkChangeLens {..}
    (.) :: forall updateA updateB updateC.
           GenChangeLens lin updateB updateC
        -> GenChangeLens lin updateA updateB
        -> GenChangeLens lin updateA updateC
    (MkChangeLens gBC uBC peBC) . (MkChangeLens gAB uAB peAB) = let
        gAC :: forall m. MonadIO m
            => Readable m (UpdateReader updateA)
            -> Readable m (UpdateReader updateC)
        gAC mra = gBC $ gAB mra
        uAC :: forall m. MonadIO m
            => updateA
            -> Readable m (UpdateReader updateA)
            -> m [updateC]
        uAC updA mrA = do
            updBs <- uAB updA mrA
            updCss <- for updBs $ \updB -> uBC updB $ \rt -> id $ gAB mrA rt
            return $ mconcat updCss
        peAC ::
               forall m. MonadIO m
            => [UpdateEdit updateC]
            -> Readable m (NL lin (UpdateReader updateA))
            -> m (Maybe [UpdateEdit updateA])
        peAC ec mra =
            getComposeM $ do
                ebs <- MkComposeM $ peBC ec $ nlLiftReadFunction @lin gAB mra
                MkComposeM $ peAB ebs mra
        in MkChangeLens gAC uAC peAC

linearPutEditsFromPutEdit ::
       forall edita editb m m' rd. (Monad m', MonadIO m)
    => (editb -> m' (Maybe [edita]))
    -> [editb]
    -> Readable m rd
    -> m' (Maybe [edita])
linearPutEditsFromPutEdit _ [] _ = getComposeM $ return []
linearPutEditsFromPutEdit putEdit (e:ee) _ =
    getComposeM $ do
        ea <- MkComposeM $ putEdit e
        eea <- MkComposeM $ linearPutEditsFromPutEdit @_ @_ @m putEdit ee nullReadable
        return $ ea ++ eea

clPutEditsFromPutEdit ::
       forall edita editb m m'. (Monad m', MonadIO m, ApplicableEdit edita)
    => (editb -> Readable m (EditReader edita) -> m' (Maybe [edita]))
    -> [editb]
    -> Readable m (EditReader edita)
    -> m' (Maybe [edita])
clPutEditsFromPutEdit _ [] _ = getComposeM $ return []
clPutEditsFromPutEdit clPutEdit (e:ee) mr =
    getComposeM $ do
        ea <- MkComposeM $ clPutEdit e mr
        eea <- MkComposeM $ clPutEditsFromPutEdit clPutEdit ee $ applyEdits ea mr
        return $ ea ++ eea

clPutEditsFromSimplePutEdit ::
       forall editA editB m. MonadIO m
    => (editB -> m (Maybe [editA]))
    -> [editB]
    -> Readable m (EditReader editA)
    -> m (Maybe [editA])
clPutEditsFromSimplePutEdit putEdit editBs _ =
    getComposeM $ do
        editAss <- for editBs $ \update -> MkComposeM $ putEdit update
        return $ mconcat editAss

isoConvertChangeLens ::
       forall updateA updateB.
       ( FullEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateB)
       , FullUpdate updateB
       , ApplicableEdit (UpdateEdit updateB)
       )
    => Bijection (UpdateSubject updateA) (UpdateSubject updateB)
    -> ChangeLens updateA updateB
isoConvertChangeLens MkIsomorphism {..} = let
    clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    clRead mr = mSubjectToReadable $ fmap isoForwards $ readableToSubject mr
    clUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [updateB]
    clUpdate _updateA mr = do
        newa <- readableToSubject mr
        getReplaceUpdatesFromSubject $ isoForwards newa
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    clPutEdits editbs mr = do
        newsubject <-
            readableToSubject $ applyEdits editbs $ mSubjectToReadable $ fmap isoForwards $ readableToSubject mr
        editas <- getReplaceEditsFromSubject $ isoBackwards newsubject
        return $ Just editas
    in MkChangeLens {..}

convertChangeLens ::
       forall updateA updateB.
       ( UpdateSubject updateA ~ UpdateSubject updateB
       , FullEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateB)
       , FullUpdate updateB
       , ApplicableEdit (UpdateEdit updateB)
       )
    => ChangeLens updateA updateB
convertChangeLens = isoConvertChangeLens id

class IsChangeLens lens where
    type LensDomain lens :: Type
    type LensRange lens :: Type
    toChangeLens :: lens -> ChangeLens (LensDomain lens) (LensRange lens)

instance IsChangeLens (ChangeLens updateA updateB) where
    type LensDomain (ChangeLens updateA updateB) = updateA
    type LensRange (ChangeLens updateA updateB) = updateB
    toChangeLens = id
