module Truth.Core.Types.Comonad where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    newtype ComonadReader (w :: * -> *) (reader :: * -> *) (t :: *) where
    {
        ReadExtract :: forall w reader t. reader t -> ComonadReader w reader t;
    };

    instance (Comonad w,SubjectReader reader) => SubjectReader (ComonadReader w reader) where
    {
        type ReaderSubject (ComonadReader w reader) = w (ReaderSubject reader);
        readFromSubject wsubj (ReadExtract reader) = readFromSubject (extract wsubj) reader;
    };
{-
    instance (Traversable f,Monad f,FullSubjectReader reader) => FullSubjectReader (OneReader f reader) where
    {
        -- pureFromReader :: ReadFunction (OneReader f reader) (f (ReaderSubject reader));
        pureFromReader = liftMaybeReadable pureFromReader;
    };
-}

    comonadReadFunction :: ReadFunction (ComonadReader w reader) reader;
    comonadReadFunction rt = readable $ ReadExtract rt;

    comonadLiftReadFunction :: ReadFunction ra rb -> ReadFunction (ComonadReader w ra) (ComonadReader w rb);
    comonadLiftReadFunction rf (ReadExtract reader) = mapReadable comonadReadFunction (rf reader);


    newtype ComonadEdit (w :: * -> *) (edit :: *) = MkComonadEdit edit;

    instance Floating edit edit => Floating (ComonadEdit w edit) (ComonadEdit w edit) where
    {
        floatingUpdate (MkComonadEdit e1) (MkComonadEdit e2) = MkComonadEdit $ floatingUpdate e1 e2;
    };

    instance Edit edit => Edit (ComonadEdit w edit) where
    {
        type EditReader (ComonadEdit w edit) = ComonadReader w (EditReader edit);
        applyEdit (MkComonadEdit edit) = comonadLiftReadFunction $ applyEdit edit;
    };

    instance InvertableEdit edit => InvertableEdit (ComonadEdit w edit) where
    {
        invertEdit (MkComonadEdit edit) = fmap (fmap MkComonadEdit) $ mapReadable comonadReadFunction $ invertEdit edit;
    };

    comonadEditFunction :: forall w edit. EditFunction () (ComonadEdit w edit) edit;
    comonadEditFunction = let
    {
        editInitial = ();

        editGet :: () -> ReadFunction (ComonadReader w (EditReader edit)) (EditReader edit);
        editGet () = comonadReadFunction;

        editUpdate (MkComonadEdit edit) () = return ((),[edit]);
    } in MkEditFunction{..};

    comonadEditLens :: EditLens () (ComonadEdit w edit) edit;
    comonadEditLens = let
    {
        editLensFunction = comonadEditFunction;
        editLensPutEdit () edit = return $ pure ((),[MkComonadEdit edit]);
    } in MkEditLens{..};
}
