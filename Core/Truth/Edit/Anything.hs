module Truth.Edit.Anything where
{
    --import Truth.Edit.WholeEdit;
    --import Truth.Edit.Edit;
    --import Truth.Edit.Read;
    import Truth.Edit.Import;

    data Anything where
    {
        MkAnything :: forall a. Info (Type_T a) -> a -> Anything;
    };

    instance HasInfo (Type_T Anything) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T Anything |])
        [
        ];
    };
{-
    data AnyRead t where
    {
        MkAnyRead :: forall reader t. (Reader reader) =>
         Info (Type_KTT reader) -> Info (Type_T (Subject reader)) -> reader t -> AnyRead (Maybe t);
    };

    instance Reader AnyRead where
    {
        type Subject AnyRead = Anything;

        -- | Make API calls when you've actually got the subject
        -- readFromM :: forall m. (Applicative m,Monad m) => m (Subject reader) -> Structure m reader;
        -- readFromM msubj reader = fmap (\subj -> readFrom subj reader) msubj;

        -- readFrom :: Anything -> (forall t. AnyRead t -> t);
        readFrom (MkAnything infoa a) (MkAnyRead infor infoa' reader) = do
        {
            MkEqualType <- matchWitness infoa infoa';
            return (readFrom a reader);
        };
    };


    data AnyEdit where
    {
        MkAnyEdit :: forall edit. (Edit edit) => Info (Type_T edit) -> Info (Type_KTT (EditReader edit)) -> edit -> AnyEdit;
    };

    instance Edit AnyEdit where
    {
        type EditReader AnyEdit = AnyRead;

        applyEdit (MkAnyEdit _te treader edit) ar@(MkAnyReader treader' ta' reader') = case matchWitness treader treader' of
        {
            Just MkEqualType -> ;
            _ -> readable ar;
        };


        FunctionConstFunction (\anya@(MkAnything ta a) -> case matchWitness tsubj ta of
        {
            Just MkEqualType -> MkAnything ta (applyConstFunction (applyEdit edit) a);
            _ -> anya;
        });

        invertEdit (MkAnyEdit te tsubj edit) (MkAnything ta a) = do
        {
            MkEqualType <- matchWitness tsubj ta;
            newa <- invertEdit edit a;
            return (MkAnyEdit te tsubj newa);
        };
    };

    instance HasInfo (Type_T AnyEdit) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T AnyEdit |])
        [
            mkFacts (MkFactZ (do
            {
                return (Edit_Inst info);
            })
            :: FactZ Edit_Inst (Type_T AnyEdit)
            )
        ];
    };

    type AnyWholeEdit = Either (WholeEdit Anything) AnyEdit;
-}
}
