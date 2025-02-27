module Changes.Core.Types.One.Read where

import Changes.Core.Import
import Changes.Core.Read

data OneReader (f :: Type -> Type) (reader :: Type -> Type) (t :: Type) where
    ReadHasOne :: forall f reader. OneReader f reader (f ())
    ReadOne :: forall f reader t. reader t -> OneReader f reader (f t)

instance forall f reader t. AllConstraint Show reader => Show (OneReader f reader t) where
    show ReadHasOne = "has"
    show (ReadOne rt) = "one " ++ allShow rt

instance forall f reader. AllConstraint Show reader => AllConstraint Show (OneReader f reader) where
    allConstraint = Dict

instance
    forall reader e.
    (Show e, WitnessConstraint Show reader) =>
    WitnessConstraint Show (OneReader (Result e) reader)
    where
    witnessConstraint ReadHasOne = Dict
    witnessConstraint (ReadOne rt) =
        case witnessConstraint @_ @Show rt of
            Dict -> Dict

instance forall reader. WitnessConstraint Show reader => WitnessConstraint Show (OneReader Maybe reader) where
    witnessConstraint ReadHasOne = Dict
    witnessConstraint (ReadOne rt) =
        case witnessConstraint @_ @Show rt of
            Dict -> Dict

instance forall f reader. (Functor f, SubjectReader reader) => SubjectReader (OneReader f reader) where
    type ReaderSubject (OneReader f reader) = f (ReaderSubject reader)
    subjectToRead fsubj ReadHasOne = fmap (\_ -> ()) fsubj
    subjectToRead fsubj (ReadOne rd) = fmap (\subj -> subjectToRead subj rd) fsubj

oneReadFunctionF :: forall f reader. ReadFunctionF f (OneReader f reader) reader
oneReadFunctionF mr rt = MkComposeInner $ mr $ ReadOne rt

liftOneReadFunction ::
    forall f ra rb.
    MonadInner f =>
    ReadFunction ra rb ->
    ReadFunction (OneReader f ra) (OneReader f rb)
liftOneReadFunction _rfrarb mr ReadHasOne = mr ReadHasOne
liftOneReadFunction rfrarb (mr :: Readable m _) (ReadOne rbt) = unComposeInner $ rfrarb (oneReadFunctionF mr) rbt

instance forall f reader. (MonadInner f, FullSubjectReader reader) => FullSubjectReader (OneReader f reader) where
    readableToSubject mr = unComposeInner $ readableToSubject $ oneReadFunctionF mr
