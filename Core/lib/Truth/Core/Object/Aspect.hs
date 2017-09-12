module Truth.Core.Object.Aspect where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;


    data Aspect edit where
    {
        MkAspect :: forall edita editb. Edit editb => String -> UISpec editb -> GeneralLens edita editb -> Aspect edita;
    };

    lensFullEdit :: IOFullEdit edita => GeneralLens edita editb -> ConstraintWitness (IOFullEdit editb);
    lensFullEdit _ = error "lensFullEdit";

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
            (name,spec) -> MkAspect name spec $ tupleEditLens seledit;
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
        -- view can create object
        ;
        MkUIMaybe :: forall edit. (IOFullEdit edit) => Maybe (EditSubject edit) -> UISpec edit -> UIOne (OneWholeEdit Maybe edit);
        MkUIOneWhole :: forall f edit. (MonadOne f,IOFullEdit edit) => UISpec edit -> UIOne (OneWholeEdit f edit);
        --MkUIOne :: forall f edit. (MonadOne f,Edit edit) => UISpec edit -> UIOne (OneEdit f edit);
    };

    instance Show (UIOne edit) where
    {
        show (MkUIMaybe _ uispec) = "maybe " ++ show uispec;
        show (MkUIOneWhole uispec) = "one+whole " ++ show uispec;
        --show (MkUIOne uispec) = "one " ++ show uispec;
    };

    instance UIType UIOne where
    {
        uiWitness = $(iowitness [t|UIOne|]);
    };

    data KeyColumn edit = MkKeyColumn
    {
        kcName :: String,
        kcFunction :: ObjectFunction edit (WholeEdit String)
    };

    mapKeyColumn :: (Edit edita,Edit editb) => ObjectFunction edita editb -> KeyColumn editb -> KeyColumn edita;
    mapKeyColumn ff (MkKeyColumn n f) = MkKeyColumn n $ f <.> ff;

    data UIContextTable edit where
    {
        MkUIContextTable :: forall cont cedit iedit. (IONewItemKeyContainer cont,IOFullReader (EditReader iedit),Edit cedit,Edit iedit,HasKeyReader cont (EditReader iedit)) =>
            [KeyColumn (ContextEdit cedit iedit)] -> Aspect (ContextEdit cedit (OneWholeEdit Maybe iedit)) -> UIContextTable (ContextEdit cedit (KeyEdit cont iedit));
    };

    uiTableToContext :: forall cont iedit. (IONewItemKeyContainer cont,IOFullReader (EditReader iedit),HasKeyReader cont (EditReader iedit),Edit iedit) =>
        [KeyColumn iedit] -> Aspect (OneWholeEdit Maybe iedit) -> UISpec (KeyEdit cont iedit);
    uiTableToContext cols aspect = let
    {
        cols' = fmap (mapKeyColumn $ tupleObjectFunction EditContent) cols;
        aspect' = mapAspect contentLens aspect;
    } in MkUISpec $ MkUILens (MkUISpec $ MkUIContextTable cols' aspect') nullContextGeneralLens;

    instance Show (UIContextTable edit) where
    {
        show (MkUIContextTable cols aspect) = "context-table (" ++ intercalate ", " (fmap kcName cols) ++ ") " ++ show aspect;
    };

    instance UIType UIContextTable where
    {
        uiWitness = $(iowitness [t|UIContextTable|]);
    };
{-
    mapOneEditAspect :: forall f edit. MonadOne f =>
        (forall editb. Edit editb => UISpec editb -> UISpec (OneEdit f editb)) -> Aspect edit -> Aspect (OneEdit f edit);
    mapOneEditAspect ff (MkAspect name uispec lens) = MkAspect name (ff uispec) $ oneLiftGeneralLens lens;
-}
    mapOneWholeEditAspect :: forall f edit. (MonadOne f, IOFullEdit edit) =>
        (forall editb. IOFullEdit editb => UISpec editb -> UISpec (OneWholeEdit f editb)) -> Aspect edit -> Aspect (OneWholeEdit f edit);
    mapOneWholeEditAspect ff (MkAspect name uispec lens) = case lensFullEdit lens of
    {
        MkConstraintWitness -> MkAspect name (ff uispec) $ oneWholeLiftGeneralLens lens;
    };

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
