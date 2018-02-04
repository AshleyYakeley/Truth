module Truth.Core.Types.OneReader where

import Truth.Core.Import
import Truth.Core.Read

data OneReader (f :: * -> *) (reader :: * -> *) (t :: *) where
    ReadHasOne :: forall f reader. OneReader f reader (f ())
    ReadOne :: forall f reader t. reader t -> OneReader f reader (f t)

instance (Functor f, SubjectReader reader) => SubjectReader (OneReader f reader) where
    type ReaderSubject (OneReader f reader) = f (ReaderSubject reader)
    subjectToRead fsubj ReadHasOne = fmap (\_ -> ()) fsubj
    subjectToRead fsubj (ReadOne reader) = fmap (\subj -> subjectToRead subj reader) fsubj

oneReadFunctionF :: ReadFunctionF f (OneReader f reader) reader
oneReadFunctionF mr rt = Compose $ mr $ ReadOne rt

liftMaybeReadFunction ::
       (MonadOne f, MonadTransTunnel t) => ReadFunctionT t ra rb -> ReadFunctionT t (OneReader f ra) (OneReader f rb)
liftMaybeReadFunction _rfrarb mr ReadHasOne = lift $ mr ReadHasOne
liftMaybeReadFunction rfrarb mr (ReadOne rbt) = transComposeOne $ rfrarb (oneReadFunctionF mr) rbt

instance (Traversable f, Monad f, FullSubjectReader reader) => FullSubjectReader (OneReader f reader) where
    mutableReadToSubject mr = getCompose $ mutableReadToSubject $ oneReadFunctionF mr
