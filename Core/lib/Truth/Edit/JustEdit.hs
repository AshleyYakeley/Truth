module Truth.Edit.JustEdit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.MonadOneReader;


    newtype JustEdit (f :: * -> *) edit = MkJustEdit edit;

    instance Floating (JustEdit f edit) (JustEdit f edit);

    instance (MonadOne f,Edit edit) => Edit (JustEdit f edit) where
    {
        type EditReader (JustEdit f edit) = MonadOneReader f (EditReader edit);

        -- applyEdit :: JustEdit f edit -> ReadMap (MonadOneReader f (EditReader edit)) (MonadOneReader f (EditReader edit));
        applyEdit (MkJustEdit _edita) ReadHasOne = readable ReadHasOne;
        applyEdit (MkJustEdit edita) (ReadOne reader) = liftMaybeReadable (applyEdit edita reader);

        -- invertEdit :: JustEdit f edit -> Readable (MonadOneReader f reader) (Maybe (JustEdit f edit));    -- "Nothing" means no change
        invertEdit (MkJustEdit edita) = do
        {
            fme <- liftMaybeReadable (invertEdit edita);
            return (case getMaybeOne fme of
            {
                Just edits -> fmap MkJustEdit edits;
                _ -> [];
            });
        };
    };

    instance HasInfo JustEdit where
    {
        info = mkSimpleInfo $(iowitness[t|JustEdit|])
        [
            -- instance (MonadOne f,Edit edit) => Edit (JustEdit f edit)
            MkKnowledge $ \knowledge ejfe -> do
            {
                MkSplitInfo edit jfe <- matchInfo ejfe;
                ReflH <- testHetEquality (info @Edit) edit;
                MkSplitInfo jf editVar <- matchInfo jfe;
                MkSplitInfo j fVar <- matchInfo jf;
                ReflH <- testHetEquality (info @JustEdit) j;
                ConstraintFact <- ask knowledge $ applyInfo (info @MonadOne) fVar;
                ConstraintFact <- ask knowledge $ applyInfo (info @Edit) editVar;
                return ConstraintFact;
            }
        ];
    };
}
