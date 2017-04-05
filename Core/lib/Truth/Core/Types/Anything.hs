module Truth.Core.Types.Anything where
{
    import Truth.Core.Import;
    --import Truth.Core.Read;
    --import Truth.Core.Edit;
    --import Truth.Core.Types.Whole;


    data Anything where
    {
        MkAnything :: forall (a :: *). Info a -> a -> Anything;
    };
{-
    instance HasInfo (Type_T Anything) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T Anything |])
        [
        ];
    };
-}


{-
    data AnyRead t where
    {
        MkAnyRead :: forall reader t. (Reader reader) =>
         Info (Type_KTT reader) -> Info (Type_T (ReaderSubject reader)) -> reader t -> AnyRead (Maybe t);
    };

    instance Reader AnyRead where
    {
        type ReaderSubject AnyRead = Anything;

        -- | Make API calls when you've actually got the subject
        -- readFromM :: forall m. Monad m => m (ReaderSubject reader) -> Structure m reader;
        -- readFromM msubj reader = fmap (\subj -> readFrom subj reader) msubj;

        -- readFrom :: Anything -> (forall t. AnyRead t -> t);
        readFrom (MkAnything infoa a) (MkAnyRead infor infoa' reader) = do
        {
            Refl <- testEquality infoa infoa';
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

        applyEdit (MkAnyEdit _te treader edit) ar@(MkAnyReader treader' ta' reader') = case testEquality treader treader' of
        {
            Just Refl -> ;
            _ -> readable ar;
        };


        FunctionConstFunction (\anya@(MkAnything ta a) -> case testEquality tsubj ta of
        {
            Just Refl -> MkAnything ta (applyConstFunction (applyEdit edit) a);
            _ -> anya;
        });

        invertEdit (MkAnyEdit te tsubj edit) (MkAnything ta a) = do
        {
            Refl <- testEquality tsubj ta;
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
