module Truth.Core.Object.Aspect where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;


    data Aspect edit where
    {
        MkAspect :: forall edita editb. (IOFullEdit editb) => String -> UISpec editb -> GeneralLens edita editb -> Aspect edita;
    };

    instance Show (Aspect edit) where
    {
        show (MkAspect name _ _) = name;
    };

    mapAspect :: (Edit edita,Edit editb) => GeneralLens edita editb -> Aspect editb -> Aspect edita;
    mapAspect lens (MkAspect name spec lens') = MkAspect name spec $ lens' <.> lens;

    data UISpec (edit :: *) where
    {
        MkUISpec :: forall (t :: * -> *) (edit :: *). (Show (t edit),UIType t) => t edit -> UISpec edit;
    };

    instance Show (UISpec edit) where
    {
        show (MkUISpec tedit) = show tedit;
    };

    class UIType (t :: * -> *) where
    {
        uiWitness :: IOWitness t;
    };

    isUISpec :: forall t edit. UIType t => UISpec edit -> Maybe (t edit);
    isUISpec (MkUISpec (tedit :: t' edit)) = do
    {
        Refl <- testEquality (uiWitness @t) (uiWitness @t');
        return tedit;
    };


    data UILens edit where
    {
        MkUILens :: forall edita editb. Edit editb => UISpec editb -> GeneralLens edita editb -> UILens edita;
    };

    instance Show (UILens edit) where
    {
        show (MkUILens uispec _) = "lens " ++ show uispec;
    };

    instance UIType UILens where
    {
        uiWitness = $(iowitness [t|UILens|]);
    };


    data UIVertical edit where
    {
        MkUIVertical :: [Aspect edit] -> UIVertical edit;
    };

    instance Show (UIVertical edit) where
    {
        show (MkUIVertical aspects) = "vertical (" ++ intercalate ", " (fmap show aspects) ++ ")";
    };

    instance UIType UIVertical where
    {
        uiWitness = $(iowitness [t|UIVertical|]);
    };

    uiVertical :: [Aspect edit] -> UISpec edit;
    uiVertical aspects = MkUISpec $ MkUIVertical aspects;

    tupleEditAspects :: (TupleWitness IOFullEdit sel,FiniteTupleSelector sel) =>
        (forall edit. IOFullEdit edit => sel edit -> (String,UISpec edit)) -> [Aspect (TupleEdit sel)];
    tupleEditAspects getSpec = fmap (\(MkAnyWitness seledit) -> case tupleWitness (Proxy::Proxy IOFullEdit) seledit of
    {
        MkConstraintWitness -> case getSpec seledit of
        {
            (name,spec) -> MkAspect name spec $ MkCloseState $ tupleEditLens seledit;
        };
    }) tupleAllSelectors;


    data UICheckbox edit where
    {
        MkUICheckbox :: String -> UICheckbox (WholeEdit Bool);
    };

    instance Show (UICheckbox edit) where
    {
        show _ = "checkbox";
    };

    instance UIType UICheckbox where
    {
        uiWitness = $(iowitness [t|UICheckbox|]);
    };

    uiCheckbox :: String -> UISpec (WholeReaderEdit (WholeReader Bool));
    uiCheckbox text = MkUISpec $ MkUICheckbox text;

    data UIText edit where
    {
        MkStringUIText :: UIText (StringEdit String);
        MkTextUIText :: UIText (StringEdit Text);
    };

    instance Show (UIText edit) where
    {
        show MkStringUIText = "string";
        show MkTextUIText = "text";
    };

    instance UIType UIText where
    {
        uiWitness = $(iowitness [t|UIText|]);
    };

    data UITextEntry edit where
    {
        MkUITextEntry :: UITextEntry (WholeEdit String);
    };

    instance Show (UITextEntry edit) where
    {
        show MkUITextEntry = "text entry";
    };

    instance UIType UITextEntry where
    {
        uiWitness = $(iowitness [t|UITextEntry|]);
    };

    data UIOne edit where
    {
        MkUIMaybe :: forall edit. (IOFullEdit edit) => Maybe (EditSubject edit) -> UISpec edit -> UIOne (OneWholeEdit Maybe edit);
        MkUIOne :: forall f edit. (MonadOne f,IOFullEdit edit) => UISpec edit -> UIOne (OneWholeEdit f edit);
    };

    instance Show (UIOne edit) where
    {
        show (MkUIMaybe _ uispec) = "maybe " ++ show uispec;
        show (MkUIOne uispec) = "one " ++ show uispec;
    };

    instance UIType UIOne where
    {
        uiWitness = $(iowitness [t|UIOne|]);
    };

    data UIKeyContainer edit where
    {
        MkUIKeyContainer :: forall cont keyedit valueedit.
            (
                Show (ContainerKey cont),
                IONewItemKeyContainer cont,
                Edit keyedit,
                IOFullReader (EditReader keyedit),
                IOFullEdit valueedit,
                HasKeyReader cont (PairEditReader keyedit valueedit)
            ) =>
            UISpec valueedit -> UIKeyContainer (KeyEdit cont (PairEdit keyedit valueedit));
    };

    instance Show (UIKeyContainer edit) where
    {
        show (MkUIKeyContainer uispec) = "key " ++ show uispec;
    };

    instance UIType UIKeyContainer where
    {
        uiWitness = $(iowitness [t|UIKeyContainer|]);
    };

    mapOneWholeEditAspect :: forall f edit. (MonadOne f, IOFullEdit edit) =>
        (forall editb. IOFullEdit editb => UISpec editb -> UISpec (OneWholeEdit f editb)) -> Aspect edit -> Aspect (OneWholeEdit f edit);
    mapOneWholeEditAspect ff (MkAspect name uispec lens) = MkAspect name (ff uispec) $ toGeneralLens $ oneWholeLiftGeneralLens getMaybeOne lens;

    data UIWindowButton edit where
    {
        MkUIWindowButton :: UISpec edit -> UIWindowButton edit;
    };

    instance Show (UIWindowButton edit) where
    {
        show (MkUIWindowButton uispec) = "window " ++ show uispec;
    };

    instance UIType UIWindowButton where
    {
        uiWitness = $(iowitness [t|UIWindowButton|]);
    };
}
