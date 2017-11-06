module Truth.Core.Types.OneEdit where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.OneReader

newtype OneEdit (f :: * -> *) edit =
    MkOneEdit edit

instance Floating edit edit => Floating (OneEdit f edit) (OneEdit f edit) where
    floatingUpdate (MkOneEdit e1) (MkOneEdit e2) = MkOneEdit $ floatingUpdate e1 e2

instance (MonadOne f, Edit edit) => Edit (OneEdit f edit) where
    type EditReader (OneEdit f edit) = OneReader f (EditReader edit)
    applyEdit (MkOneEdit _edita) ReadHasOne = readable ReadHasOne
    applyEdit (MkOneEdit edita) (ReadOne reader) = liftMaybeReadable (applyEdit edita reader)

instance (MonadOne f, InvertableEdit edit) => InvertableEdit (OneEdit f edit) where
    invertEdit (MkOneEdit edita) = do
        fme <- liftMaybeReadable (invertEdit edita)
        return
            (case getMaybeOne fme of
                 Just edits -> fmap MkOneEdit edits
                 _ -> [])

oneLiftEditFunction ::
       forall f state edita editb. (MonadOne f)
    => EditFunction state edita editb
    -> EditFunction state (OneEdit f edita) (OneEdit f editb)
oneLiftEditFunction ff =
    MkEditFunction
    { editAccess = editAccess ff
    , editGet = \curstate -> liftMaybeReadFunction (editGet ff curstate)
    , editUpdate =
          \(MkOneEdit edita) oldstate -> do
              fr <- liftMaybeReadable $ editUpdate ff edita oldstate
              return $
                  case retrieveOne fr of
                      SuccessResult (newstate, editBs) -> (newstate, fmap MkOneEdit editBs)
                      FailureResult _fx -> (oldstate, [])
    }

oneLiftEditLens ::
       forall f state edita editb. MonadOne f
    => EditLens state edita editb
    -> EditLens state (OneEdit f edita) (OneEdit f editb)
oneLiftEditLens lens =
    MkEditLens
    { editLensFunction = oneLiftEditFunction (editLensFunction lens)
    , editLensPutEdit =
          \oldstate (MkOneEdit editb) -> do
              fmeditas <- liftMaybeReadable $ editLensPutEdit lens oldstate editb
              return $ do
                  meditas <- getMaybeOne fmeditas
                  (newstate, editas) <- meditas
                  return (newstate, fmap MkOneEdit editas)
    }

oneLiftGeneralLens :: MonadOne f => GeneralLens edita editb -> GeneralLens (OneEdit f edita) (OneEdit f editb)
oneLiftGeneralLens (MkCloseState lens) = MkCloseState $ oneLiftEditLens lens
