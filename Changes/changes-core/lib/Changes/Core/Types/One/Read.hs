module Changes.Core.Types.One.Read where

import Changes.Core.Import
import Changes.Core.Read

data OneReader (f :: Type -> Type) (reader :: Type -> Type) (t :: Type) where
    ReadHasOne :: forall f reader. OneReader f reader (f ())
    ReadOne :: forall f reader t. reader t -> OneReader f reader (f t)

instance AllWitnessConstraint Show reader => Show (OneReader f reader t) where
    show ReadHasOne = "has"
    show (ReadOne rt) = "one " ++ showAllWitness rt

instance AllWitnessConstraint Show reader => AllWitnessConstraint Show (OneReader f reader) where
    allWitnessConstraint = Dict

instance (Show e, WitnessConstraint Show reader) => WitnessConstraint Show (OneReader (Result e) reader) where
    witnessConstraint ReadHasOne = Dict
    witnessConstraint (ReadOne rt) =
        case witnessConstraint @_ @Show rt of
            Dict -> Dict

instance (Show e, WitnessConstraint Show reader) => WitnessConstraint Show (OneReader Maybe reader) where
    witnessConstraint ReadHasOne = Dict
    witnessConstraint (ReadOne rt) =
        case witnessConstraint @_ @Show rt of
            Dict -> Dict

instance (Functor f, SubjectReader reader) => SubjectReader (OneReader f reader) where
    type ReaderSubject (OneReader f reader) = f (ReaderSubject reader)
    subjectToRead fsubj ReadHasOne = fmap (\_ -> ()) fsubj
    subjectToRead fsubj (ReadOne rd) = fmap (\subj -> subjectToRead subj rd) fsubj

oneReadFunctionF :: ReadFunctionF f (OneReader f reader) reader
oneReadFunctionF mr rt = MkComposeInner $ mr $ ReadOne rt

liftOneReadFunction ::
       forall f ra rb. MonadInner f
    => ReadFunction ra rb
    -> ReadFunction (OneReader f ra) (OneReader f rb)
liftOneReadFunction _rfrarb mr ReadHasOne = mr ReadHasOne
liftOneReadFunction rfrarb (mr :: Readable m _) (ReadOne rbt) = getComposeInner $ rfrarb (oneReadFunctionF mr) rbt

instance (MonadInner f, FullSubjectReader reader) => FullSubjectReader (OneReader f reader) where
    readableToSubject mr = getComposeInner $ readableToSubject $ oneReadFunctionF mr
