module Truth.Core.Types.Anything where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    --import Truth.Core.Edit;
    --import Truth.Core.Types.EitherWhole;


    data Anything where
    {
        MkAnything :: forall (a :: *). Info a -> a -> Anything;
    };

    instance HasInfo Anything where
    {
        info = mkSimpleInfo $(iowitness[t|Anything|]) [$(declInfo [d|
        |])];
    };

    data AnyReader t where
    {
        MkAnyReader :: forall reader t. (Reader reader) => Info reader -> Info (ReaderSubject reader) -> reader t -> AnyReader (Maybe t);
    };

    instance Reader AnyReader where
    {
        type ReaderSubject AnyReader = Anything;

        readFrom (MkAnything infoa a) (MkAnyReader _infor infoa' reader) = do
        {
            Refl <- testEquality infoa infoa';
            return (readFrom a reader);
        };
    };

    $(return []);
    instance HasInfo AnyReader where
    {
        info = mkSimpleInfo $(iowitness[t|AnyReader|]) [$(declInfo [d|
            instance Reader AnyReader where
            {
                type ReaderSubject AnyReader = Anything;
            };
        |])];
    };

{-
    data AnyEdit where
    {
        MkAnyEdit :: forall edit. (Edit edit) => Info edit -> Info (EditReader edit) -> edit -> AnyEdit;
    };

    instance Edit AnyEdit where
    {
        type EditReader AnyEdit = AnyReader;

        applyEdit (MkAnyEdit _te treader edit) ar@(MkAnyReaderer treader' ta' reader') = case testEquality treader treader' of
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

    instance HasInfo AnyEdit where
    {
        info = mkSimpleInfo $(iowitness[t|AnyEdit|]) [$(declInfo [d|
            instance Edit AnyEdit where
            {
                type EditReader AnyEdit = AnyReader;
            };
        |])];
    };

    type AnyWholeEdit = EitherWholeEdit AnyEdit;
-}
}
