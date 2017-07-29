module Truth.Core.Types.OneReader where
{
    import Truth.Core.Import;
    import Truth.Core.Read;


    data OneReader (f :: * -> *) (reader :: * -> *) (t :: *) where
    {
        ReadHasOne :: forall f reader. OneReader f reader (f ());
        ReadOne :: forall f reader t. reader t -> OneReader f reader (f t);
    };

    instance (Functor f,Reader reader) => Reader (OneReader f reader) where
    {
        type ReaderSubject (OneReader f reader) = f (ReaderSubject reader);

        -- readFrom :: ReaderSubject (OneReader f reader) -> (forall t. OneReader f reader t -> t);
        readFrom fsubj ReadHasOne = fmap (\_ -> ()) fsubj;
        readFrom fsubj (ReadOne reader) = fmap (\subj -> readFrom subj reader) fsubj;
    };

    oneReadFunctionF :: ReadFunctionF f (OneReader f reader) reader;
    oneReadFunctionF = readable . ReadOne;

    liftMaybeReadable :: (Traversable f,Monad f) => MapReadable readable => readable reader a -> readable (OneReader f reader) (f a);
    liftMaybeReadable = mapReadableF oneReadFunctionF;

    liftMaybeReadFunction :: (ReadableConstraint c,Traversable f,Monad f) => GenReadFunction c ra rb -> GenReadFunction c (OneReader f ra) (OneReader f rb);
    liftMaybeReadFunction _rfrarb ReadHasOne = readable ReadHasOne;
    liftMaybeReadFunction rfrarb (ReadOne rt) = liftMaybeReadable (rfrarb rt);

    instance (Traversable f,Monad f,ReadableConstraint c,GenFullReader c reader) => GenFullReader c (OneReader f reader) where
    {
        genFromReader = liftMaybeReadable genFromReader;
    };

    $(return []);
    instance HasTypeInfo OneReader where
    {
        typeWitness = $(generateWitness [t|OneReader|]);
        typeName _ = "OneReader";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance (Functor f,Reader reader) => Reader (OneReader f reader) where
            {
                type ReaderSubject (OneReader f reader) = f (ReaderSubject reader);
            };
            instance (Traversable f,Monad f,ReadableConstraint c,GenFullReader c reader) => GenFullReader c (OneReader f reader);
        |]);
    };
}
