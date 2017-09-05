module Truth.Core.Types.Comonad where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    newtype ComonadReader (w :: * -> *) (reader :: * -> *) (t :: *) where
    {
        ReadExtract :: forall w reader t. reader t -> ComonadReader w reader t;
    };

    instance (Comonad w,Reader reader) => Reader (ComonadReader w reader) where
    {
        type ReaderSubject (ComonadReader w reader) = w (ReaderSubject reader);
        readFrom wsubj (ReadExtract reader) = readFrom (extract wsubj) reader;
    };
{-
    instance (Traversable f,Monad f,PureFullReader reader) => PureFullReader (OneReader f reader) where
    {
        -- pureFromReader :: PureReadFunction (OneReader f reader) (f (ReaderSubject reader));
        pureFromReader = liftMaybeReadable pureFromReader;
    };
-}

    comonadReadFunction :: ReadFunction c (ComonadReader w reader) reader;
    comonadReadFunction rt = readable $ ReadExtract rt;

    comonadLiftReadFunction :: ReadableConstraint c => ReadFunction c ra rb -> ReadFunction c (ComonadReader w ra) (ComonadReader w rb);
    comonadLiftReadFunction rf (ReadExtract reader) = mapReadable comonadReadFunction (rf reader);


    newtype ComonadEdit (w :: * -> *) (edit :: *) = MkComonadEdit edit;

    instance Floating edit edit => Floating (ComonadEdit w edit) (ComonadEdit w edit) where
    {
        floatingUpdate (MkComonadEdit e1) (MkComonadEdit e2) = MkComonadEdit $ floatingUpdate e1 e2;
    };

    instance (Comonad w,Edit edit) => Edit (ComonadEdit w edit) where
    {
        type EditReader (ComonadEdit w edit) = ComonadReader w (EditReader edit);
        applyEdit (MkComonadEdit edit) = comonadLiftReadFunction $ applyEdit edit;
        invertEdit (MkComonadEdit edit) = fmap (fmap MkComonadEdit) $ mapReadable comonadReadFunction $ invertEdit edit;
    };

    comonadEditFunction :: forall w edit. PureEditFunction () (ComonadEdit w edit) edit;
    comonadEditFunction = let
    {
        editInitial = ();

        editGet :: () -> PureReadFunction (ComonadReader w (EditReader edit)) (EditReader edit);
        editGet () = comonadReadFunction;

        editUpdate (MkComonadEdit edit) () = return ((),[edit]);
    } in MkEditFunction{..};

    comonadEditLens :: Applicative m => PureEditLens' m () (ComonadEdit w edit) edit;
    comonadEditLens = let
    {
        editLensFunction = comonadEditFunction;
        editLensPutEdit () edit = return $ pure ((),[MkComonadEdit edit]);
    } in MkEditLens{..};
}
