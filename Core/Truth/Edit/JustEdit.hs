module Truth.Edit.JustEdit where
{
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    newtype JustEdit (f :: * -> *) edit = MkJustEdit edit;

    instance (FunctorOne f,Edit edit) => Edit (JustEdit f edit) where
    {
        type Subject (JustEdit f edit) = f (Subject edit);

        applyEdit (MkJustEdit edita) = cfmap (applyEdit edita);

        invertEdit (MkJustEdit edita) molda = case retrieveOne molda of
        {
            SuccessResult olda -> fmap MkJustEdit (invertEdit edita olda);
            _ -> Nothing;
        };
    };

    instance HasInfo (Type_KKTTKTT JustEdit) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KKTTKTT JustEdit |])
        [
            -- instance (FunctorOne f,Edit edit) => Edit (JustEdit f edit)
            mkFacts (MkFactS (\tf -> MkFactS (\tedit -> MkFactZ (do
            {
                Edit_Inst tsubj <- matchProp $(type1[t|Edit_Inst|]) tedit;
                FullEdit_Inst <- matchProp $(type1[t|FullEdit_Inst|]) tedit;
                HasNewValue_Inst <- matchProp $(type1[t|HasNewValue_Inst|]) tsubj;
                FunctorOne_Inst <- matchProp $(type1[t|FunctorOne_Inst|]) tf;
                return (Edit_Inst (applyInfo tf tsubj));
            })))
            :: FactS (FactS FactZ) Edit_Inst (Type_KKTTKTT JustEdit)
            )
        ];
    };
}
