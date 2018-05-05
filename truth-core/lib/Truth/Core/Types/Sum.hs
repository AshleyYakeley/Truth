module Truth.Core.Types.Sum where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

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

sumRightEditFunction ::
       forall editl editr. (EditReader editl ~ EditReader editr)
    => EditFunction editr (SumEdit editl editr)
sumRightEditFunction = let
    efGet :: ReadFunctionT IdentityT (EditReader editr) (EditReader editr)
    efGet mr = remonadMutableRead lift mr
    efUpdate ::
           forall m. MonadIO m
        => editr
        -> MutableRead m (EditReader editr)
        -> IdentityT m [SumEdit editl editr]
    efUpdate er _ = return [SumEditRight er]
    in MkCloseUnlift identityUnlift MkAnEditFunction {..}
