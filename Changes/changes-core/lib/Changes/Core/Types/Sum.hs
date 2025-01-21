module Changes.Core.Types.Sum
    ( SumEdit (..)
    , SumUpdate (..)
    )
where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Read

data SumEdit ea eb
    = SumEditLeft ea
    | SumEditRight eb

instance (Show ea, Show eb) => Show (SumEdit ea eb) where
    show (SumEditLeft e) = "left " ++ show e
    show (SumEditRight e) = "right " ++ show e

instance (FloatingOn ea ea, FloatingOn eb eb) => FloatingOn (SumEdit ea eb) (SumEdit ea eb) where
    floatingUpdate (SumEditLeft e1) (SumEditLeft e2) = SumEditLeft $ floatingUpdate e1 e2
    floatingUpdate (SumEditRight e1) (SumEditRight e2) = SumEditRight $ floatingUpdate e1 e2
    floatingUpdate _ t = t

type instance EditReader (SumEdit ea _) = EditReader ea

instance (ApplicableEdit ea, ApplicableEdit eb, EditReader ea ~ EditReader eb) => ApplicableEdit (SumEdit ea eb) where
    applyEdit (SumEditLeft edit) = applyEdit edit
    applyEdit (SumEditRight edit) = applyEdit edit

instance
    (ApplicableEdit ea, ApplicableEdit eb, InvertibleEdit ea, InvertibleEdit eb, EditReader ea ~ EditReader eb) =>
    InvertibleEdit (SumEdit ea eb)
    where
    invertEdit (SumEditLeft edit) mr = fmap (fmap SumEditLeft) $ invertEdit edit mr
    invertEdit (SumEditRight edit) mr = fmap (fmap SumEditRight) $ invertEdit edit mr

instance
    ( FullSubjectReader (EditReader ea)
    , ApplicableEdit ea
    , ApplicableEdit eb
    , SubjectReader (EditReader ea)
    , EditReader ea ~ EditReader eb
    ) =>
    SubjectMapEdit (SumEdit ea eb)

instance (FullEdit ea, ApplicableEdit eb, EditReader ea ~ EditReader eb) => FullEdit (SumEdit ea eb) where
    replaceEdit mr write = replaceEdit mr (\edit -> write $ SumEditLeft edit)

data SumUpdate updateA updateB
    = SumUpdateLeft updateA
    | SumUpdateRight updateB

type instance UpdateEdit (SumUpdate updateA updateB) = SumEdit (UpdateEdit updateA) (UpdateEdit updateB)

instance (IsUpdate updateA, IsUpdate updateB) => IsUpdate (SumUpdate updateA updateB) where
    editUpdate (SumEditLeft edit) = SumUpdateLeft $ editUpdate edit
    editUpdate (SumEditRight edit) = SumUpdateRight $ editUpdate edit

instance (IsEditUpdate updateA, IsEditUpdate updateB) => IsEditUpdate (SumUpdate updateA updateB) where
    updateEdit (SumUpdateLeft update) = SumEditLeft $ updateEdit update
    updateEdit (SumUpdateRight update) = SumEditRight $ updateEdit update
