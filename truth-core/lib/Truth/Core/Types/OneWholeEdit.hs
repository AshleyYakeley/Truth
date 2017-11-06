module Truth.Core.Types.OneWholeEdit where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.OneEdit
import Truth.Core.Types.OneReader
import Truth.Core.Types.Sum
import Truth.Core.Types.SumWhole
import Truth.Core.Types.Whole

type OneWholeEdit (f :: * -> *) edit = SumWholeEdit (OneEdit f edit)

type MaybeEdit edit = OneWholeEdit Maybe edit

oneWholeLiftEditFunction ::
       forall f state edita editb. (MonadOne f, SubjectReader (EditReader edita), FullSubjectReader (EditReader editb))
    => EditFunction state edita editb
    -> EditFunction state (OneWholeEdit f edita) (OneWholeEdit f editb)
oneWholeLiftEditFunction = sumWholeLiftEditFunction . oneLiftEditFunction

-- | suitable for Results; trying to put a failure code will be rejected
oneWholeLiftGeneralLens ::
       forall f edita editb. (MonadOne f, FullSubjectReader (EditReader edita), Edit edita, FullEdit editb)
    => GeneralLens edita editb
    -> GeneralLens (OneWholeEdit f edita) (OneWholeEdit f editb)
oneWholeLiftGeneralLens (MkCloseState (lens :: EditLens state edita editb)) =
    MkCloseState $ sumWholeLiftEditLens pushback (oneLiftEditLens lens)
  where
    ff1 :: forall a. state -> f (state, a) -> (state, f a)
    ff1 oldstate fsa =
        case retrieveOne fsa of
            FailureResult (MkLimit fx) -> (oldstate, fx)
            SuccessResult (newstate, a) -> (newstate, fmap (\_ -> a) fsa)
    pushback ::
           state
        -> f (EditSubject editb)
        -> Readable (OneReader f (EditReader edita)) (Maybe (state, f (EditSubject edita)))
    pushback oldstate fb =
        case retrieveOne fb of
            FailureResult (MkLimit fx) -> return $ return (oldstate, fx)
            SuccessResult b ->
                fmap (fmap (ff1 oldstate) . sequenceA) $
                liftMaybeReadable $ do
                    editbs <- getReplaceEditsM b
                    fstateedita <- editLensPutEdits lens oldstate editbs
                    for fstateedita $ \(newstate, editas) -> do
                        a <- mapReadable (applyEdits editas) subjectFromReader
                        return (newstate, a)

mustExistOneEditFunction ::
       forall f edit. (MonadOne f, FullEdit edit)
    => String
    -> PureEditFunction (OneWholeEdit f edit) edit
mustExistOneEditFunction err =
    let editAccess :: IOStateAccess ()
        editAccess = unitStateAccess
        editGet :: () -> EditReader edit t -> Readable (OneReader f (EditReader edit)) t
        editGet () reader = do
            ft <- readable $ ReadOne reader
            case retrieveOne ft of
                SuccessResult t -> return t
                FailureResult _ -> liftIO $ fail $ err ++ ": not found"
        editUpdate :: OneWholeEdit f edit -> () -> Readable (OneReader f (EditReader edit)) ((), [edit])
        editUpdate (SumEditLeft (MkWholeEdit ft)) () =
            case retrieveOne ft of
                SuccessResult t -> do
                    edits <- getReplaceEditsM t
                    return $ pure edits
                FailureResult _ -> liftIO $ fail $ err ++ ": deleted"
        editUpdate (SumEditRight (MkOneEdit edit)) () = return $ pure [edit]
    in MkEditFunction {..}

mustExistOneEditLens ::
       forall f edit. (MonadOne f, FullEdit edit)
    => String
    -> PureEditLens (OneWholeEdit f edit) edit
mustExistOneEditLens err =
    let editLensFunction = mustExistOneEditFunction err
        editLensPutEdit :: () -> edit -> Readable (OneReader f (EditReader edit)) (Maybe ((), [OneWholeEdit f edit]))
        editLensPutEdit () edit = return $ Just $ pure [SumEditRight $ MkOneEdit edit]
    in MkEditLens {..}

mustExistOneGeneralLens ::
       forall f edit. (MonadOne f, FullEdit edit)
    => String
    -> GeneralLens (OneWholeEdit f edit) edit
mustExistOneGeneralLens err = MkCloseState $ mustExistOneEditLens err
