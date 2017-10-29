module Truth.Core.Types.OneReader where
{
    import Truth.Core.Import;
    import Truth.Core.Read;


    data OneReader (f :: * -> *) (reader :: * -> *) (t :: *) where
    {
        ReadHasOne :: forall f reader. OneReader f reader (f ());
        ReadOne :: forall f reader t. reader t -> OneReader f reader (f t);
    };

    instance (Functor f,SubjectReader reader) => SubjectReader (OneReader f reader) where
    {
        type ReaderSubject (OneReader f reader) = f (ReaderSubject reader);

        -- readFromSubject :: ReaderSubject (OneReader f reader) -> (forall t. OneReader f reader t -> t);
        readFromSubject fsubj ReadHasOne = fmap (\_ -> ()) fsubj;
        readFromSubject fsubj (ReadOne reader) = fmap (\subj -> readFromSubject subj reader) fsubj;
    };

    oneReadFunctionF :: ReadFunctionF f (OneReader f reader) reader;
    oneReadFunctionF = readable . ReadOne;

    liftMaybeReadable :: (Traversable f,Monad f) => MapReadable readable => readable reader a -> readable (OneReader f reader) (f a);
    liftMaybeReadable = mapReadableF oneReadFunctionF;

    liftMaybeReadFunction :: (Traversable f,Monad f) => ReadFunction ra rb -> ReadFunction (OneReader f ra) (OneReader f rb);
    liftMaybeReadFunction _rfrarb ReadHasOne = readable ReadHasOne;
    liftMaybeReadFunction rfrarb (ReadOne rt) = liftMaybeReadable (rfrarb rt);

    instance (Traversable f,Monad f,FullSubjectReader reader) => FullSubjectReader (OneReader f reader) where
    {
        subjectFromReader = liftMaybeReadable subjectFromReader;
    };
}
