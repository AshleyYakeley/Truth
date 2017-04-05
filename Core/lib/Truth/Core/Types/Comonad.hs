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
    instance (Traversable f,Monad f,FullReader reader) => FullReader (OneReader f reader) where
    {
        -- fromReader :: ReadFunction (OneReader f reader) (f (ReaderSubject reader));
        fromReader = liftMaybeReadable fromReader;
    };
-}
    $(return []);
    instance HasInfo ComonadReader where
    {
        info = mkSimpleInfo $(iowitness[t|ComonadReader|]) [$(declInfo [d|
            instance (Comonad w,Reader reader) => Reader (ComonadReader w reader);
        |])];
    };

    comonadReadFunction :: ReadFunction (ComonadReader w reader) reader;
    comonadReadFunction rt = readable $ ReadExtract rt;

    comonadLiftReadFunction :: ReadFunction ra rb -> ReadFunction (ComonadReader w ra) (ComonadReader w rb);
    comonadLiftReadFunction rf (ReadExtract reader) = mapReadable comonadReadFunction (rf reader);


    newtype ComonadEdit (w :: * -> *) (edit :: *) = MkComonadEdit edit;

    instance Floating (ComonadEdit w edit) (ComonadEdit w edit);

    instance (Comonad w,Edit edit) => Edit (ComonadEdit w edit) where
    {
        type EditReader (ComonadEdit w edit) = ComonadReader w (EditReader edit);
        applyEdit (MkComonadEdit edit) = comonadLiftReadFunction $ applyEdit edit;
        invertEdit (MkComonadEdit edit) = fmap (fmap MkComonadEdit) $ mapReadable comonadReadFunction $ invertEdit edit;
    };

    $(return []);
    instance HasInfo ComonadEdit where
    {
        info = mkSimpleInfo $(iowitness[t|ComonadEdit|]) [$(declInfo [d|
            instance (Comonad w,Edit edit) => Edit (ComonadEdit w edit);
        |])];
    };

    comonadEditFunction :: forall w edit. EditFunction (ComonadEdit w edit) edit;
    comonadEditFunction = let
    {
        editGet :: ReadFunction (ComonadReader w (EditReader edit)) (EditReader edit);
        editGet = comonadReadFunction;

        editUpdate (MkComonadEdit edit) = [edit];
    } in MkEditFunction{..};

    comonadEditLens :: Applicative m => EditLens' m (ComonadEdit w edit) edit;
    comonadEditLens = let
    {
        editLensFunction = comonadEditFunction;
        editLensPutEdit edit = return $ pure $ MkComonadEdit edit;
    } in MkEditLens{..};
}
