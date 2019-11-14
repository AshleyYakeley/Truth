module Truth.Core.Types.Sum where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Resource

data SumEdit ea eb
    = SumEditLeft ea
    | SumEditRight eb

instance (Show ea, Show eb) => Show (SumEdit ea eb) where
    show (SumEditLeft e) = "left " ++ show e
    show (SumEditRight e) = "right " ++ show e

instance (Floating ea ea, Floating eb eb) => Floating (SumEdit ea eb) (SumEdit ea eb) where
    floatingUpdate (SumEditLeft e1) (SumEditLeft e2) = SumEditLeft $ floatingUpdate e1 e2
    floatingUpdate (SumEditRight e1) (SumEditRight e2) = SumEditRight $ floatingUpdate e1 e2
    floatingUpdate _ t = t

type instance EditReader (SumEdit ea eb) = EditReader ea

instance (ApplicableEdit ea, ApplicableEdit eb, EditReader ea ~ EditReader eb) => ApplicableEdit (SumEdit ea eb) where
    applyEdit (SumEditLeft edit) = applyEdit edit
    applyEdit (SumEditRight edit) = applyEdit edit

instance (ApplicableEdit ea, ApplicableEdit eb, InvertibleEdit ea, InvertibleEdit eb, EditReader ea ~ EditReader eb) =>
             InvertibleEdit (SumEdit ea eb) where
    invertEdit (SumEditLeft edit) mr = fmap (fmap SumEditLeft) $ invertEdit edit mr
    invertEdit (SumEditRight edit) mr = fmap (fmap SumEditRight) $ invertEdit edit mr

instance (FullSubjectReader (EditReader ea), ApplicableEdit ea, ApplicableEdit eb, EditReader ea ~ EditReader eb) =>
             SubjectMapEdit (SumEdit ea eb)

instance (FullEdit ea, ApplicableEdit eb, EditReader ea ~ EditReader eb) => FullEdit (SumEdit ea eb) where
    replaceEdit mr write = replaceEdit mr (\edit -> write $ SumEditLeft edit)

data SumUpdate updateA updateB
    = SumUpdateLeft updateA
    | SumUpdateRight updateB

instance (IsUpdate updateA, IsUpdate updateB) => IsUpdate (SumUpdate updateA updateB) where
    type UpdateEdit (SumUpdate updateA updateB) = SumEdit (UpdateEdit updateA) (UpdateEdit updateB)
    editUpdate (SumEditLeft edit) = SumUpdateLeft $ editUpdate edit
    editUpdate (SumEditRight edit) = SumUpdateRight $ editUpdate edit

instance (IsEditUpdate updateA, IsEditUpdate updateB) => IsEditUpdate (SumUpdate updateA updateB) where
    updateEdit (SumUpdateLeft update) = SumEditLeft $ updateEdit update
    updateEdit (SumUpdateRight update) = SumEditRight $ updateEdit update

sumRightUpdateFunction ::
       forall updateA updateB. (UpdateReader updateA ~ UpdateReader updateB)
    => UpdateFunction updateB (SumUpdate updateA updateB)
sumRightUpdateFunction = let
    ufGet :: ReadFunction (UpdateReader updateB) (UpdateReader updateB)
    ufGet mr = mr
    ufUpdate ::
           forall m. MonadIO m
        => updateB
        -> MutableRead m (UpdateReader updateB)
        -> m [SumUpdate updateA updateB]
    ufUpdate update _ = return [SumUpdateRight update]
    in MkRunnable2 cmEmpty MkAnUpdateFunction {..}
