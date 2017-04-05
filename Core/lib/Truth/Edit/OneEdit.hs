module Truth.Edit.OneEdit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.MonadOneReader;


    newtype OneEdit (f :: * -> *) edit = MkOneEdit edit;

    instance Floating (OneEdit f edit) (OneEdit f edit);

    instance (MonadOne f,Edit edit) => Edit (OneEdit f edit) where
    {
        type EditReader (OneEdit f edit) = MonadOneReader f (EditReader edit);

        -- applyEdit :: OneEdit f edit -> ReadMap (MonadOneReader f (EditReader edit)) (MonadOneReader f (EditReader edit));
        applyEdit (MkOneEdit _edita) ReadHasOne = readable ReadHasOne;
        applyEdit (MkOneEdit edita) (ReadOne reader) = liftMaybeReadable (applyEdit edita reader);

        -- invertEdit :: OneEdit f edit -> Readable (MonadOneReader f reader) (Maybe (OneEdit f edit));    -- "Nothing" means no change
        invertEdit (MkOneEdit edita) = do
        {
            fme <- liftMaybeReadable (invertEdit edita);
            return (case getMaybeOne fme of
            {
                Just edits -> fmap MkOneEdit edits;
                _ -> [];
            });
        };
    };

    $(return []);
    instance HasInfo OneEdit where
    {
        info = mkSimpleInfo $(iowitness[t|OneEdit|]) [$(declInfo [d|
            instance (MonadOne f,Edit edit) => Edit (OneEdit f edit);
        |])];
    };
}
