module Truth.Edit.JustEdit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.MaybeReader;


    newtype JustEdit (f :: * -> *) edit = MkJustEdit edit;

    instance Floating (JustEdit f edit) (JustEdit f edit);

    instance (FunctorOne f,Edit edit) => Edit (JustEdit f edit) where
    {
        type EditReader (JustEdit f edit) = MaybeReader f (EditReader edit);

        -- applyEdit :: JustEdit f edit -> ReadMap (MaybeReader f (EditReader edit)) (MaybeReader f (EditReader edit));
        applyEdit (MkJustEdit _edita) ReadOther = readable ReadOther;
        applyEdit (MkJustEdit _edita) ReadIsJust = readable ReadIsJust;
        applyEdit (MkJustEdit edita) (ReadWholeJust reader) = liftJustReadable (applyEdit edita reader);

        -- invertEdit :: JustEdit f edit -> Readable (MaybeReader f reader) (Maybe (JustEdit f edit));    -- "Nothing" means no change
        invertEdit (MkJustEdit edita) = do
        {
            fme <- liftJustReadable (invertEdit edita);
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
            -- instance (FunctorOne f,Edit edit) => Edit (JustEdit f edit)
            MkKnowledge $ \knowledge ejfe -> do
            {
                MkSplitInfo edit jfe <- matchInfo ejfe;
                ReflH <- testHetEquality (info @Edit) edit;
                MkSplitInfo jf editVar <- matchInfo jfe;
                MkSplitInfo j fVar <- matchInfo jf;
                ReflH <- testHetEquality (info @JustEdit) j;
                ConstraintFact <- ask knowledge $ applyInfo (info @FunctorOne) fVar;
                ConstraintFact <- ask knowledge $ applyInfo (info @Edit) editVar;
                return ConstraintFact;
            }
        ];
    };
}
