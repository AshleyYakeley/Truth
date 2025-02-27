module Changes.Core.Model.ReferenceEdit
    ( ReferenceReader (..)
    , ReferenceEdit
    , ReferenceUpdate
    , referenceChangeLens
    , referenceLiftChangeLens
    )
where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model.EditContext
import Changes.Core.Model.Reference
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types

data ReferenceReader edit t where
    ReadReferenceResourceContext :: ReferenceReader edit ResourceContext
    ReadReference :: ReferenceReader edit (Reference edit)

instance Show (ReferenceReader edit t) where
    show ReadReferenceResourceContext = "reference context"
    show ReadReference = "reference"

instance AllConstraint Show (ReferenceReader edit) where
    allConstraint = Dict

instance (c (Reference edit), c ResourceContext) => WitnessConstraint c (ReferenceReader edit) where
    witnessConstraint ReadReferenceResourceContext = Dict
    witnessConstraint ReadReference = Dict

instance SubjectReader (EditReader edit) => SubjectReader (ReferenceReader edit) where
    type ReaderSubject (ReferenceReader edit) = EditSubject edit
    subjectToRead _ ReadReferenceResourceContext = emptyResourceContext
    subjectToRead subj ReadReference =
        mapReference (fromReadOnlyRejectingChangeLens @(EditUpdate edit)) $ constantReference subj

instance FullSubjectReader (EditReader edit) => FullSubjectReader (ReferenceReader edit) where
    readableToSubject mr = do
        rc <- mr ReadReferenceResourceContext
        obj <- mr ReadReference
        liftIO $ runResource rc obj $ \anobj -> readableToSubject $ refRead anobj

type ReferenceEdit edit = ConstEdit (ReferenceReader edit)

type ReferenceUpdate update = EditUpdate (ReferenceEdit (UpdateEdit update))

referenceChangeLens :: forall update. ChangeLens (ReferenceUpdate update) update
referenceChangeLens = let
    clRead :: ReadFunction (ReferenceReader (UpdateEdit update)) (UpdateReader update)
    clRead mr rt = do
        rc <- mr ReadReferenceResourceContext
        obj <- mr ReadReference
        liftIO $ runResource rc obj $ \anobj -> refRead anobj rt
    clUpdate :: forall m. ReferenceUpdate update -> Readable m (ReferenceReader (UpdateEdit update)) -> m [update]
    clUpdate update _ = never update
    clPutEdits ::
        forall m.
        MonadIO m =>
        [UpdateEdit update] ->
        Readable m (EditReader (ReferenceEdit (UpdateEdit update))) ->
        m (Maybe [ReferenceEdit (UpdateEdit update)])
    clPutEdits edits mr =
        case nonEmpty edits of
            Nothing -> return $ Just []
            Just edits' -> do
                rc <- mr ReadReferenceResourceContext
                obj <- mr ReadReference
                liftIO
                    $ runResource rc obj
                    $ \anobj -> do
                        maction <- refEdit anobj edits'
                        case maction of
                            Just action -> action noEditSource
                            Nothing -> liftIO $ fail "referenceChangeLens: failed"
                return $ Just []
    in MkChangeLens{..}

referenceLiftChangeLens ::
    forall updateA updateB.
    ChangeLens updateA updateB ->
    ChangeLens (ReferenceUpdate updateA) (ReferenceUpdate updateB)
referenceLiftChangeLens lens = let
    clRead :: ReadFunction (ReferenceReader (UpdateEdit updateA)) (ReferenceReader (UpdateEdit updateB))
    clRead mr ReadReferenceResourceContext = mr ReadReferenceResourceContext
    clRead mr ReadReference = do
        reference <- mr ReadReference
        return $ mapReference lens reference
    clUpdate ::
        forall m.
        ReferenceUpdate updateA ->
        Readable m (ReferenceReader (UpdateEdit updateA)) ->
        m [ReferenceUpdate updateB]
    clUpdate update _ = never update
    clPutEdits ::
        forall m.
        MonadIO m =>
        [ReferenceEdit (UpdateEdit updateB)] ->
        Readable m (ReferenceReader (UpdateEdit updateA)) ->
        m (Maybe [ReferenceEdit (UpdateEdit updateA)])
    clPutEdits [] _ = return $ Just []
    clPutEdits (edit : _) _ = never edit
    in MkChangeLens{..}
