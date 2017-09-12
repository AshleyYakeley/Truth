module Truth.Core.UI.One where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.UI.Specifier;


    data UIOne edit where
    {
        -- view can create object
        ;
        MkUIMaybe :: forall edit. (FullEdit edit) => Maybe (EditSubject edit) -> UISpec edit -> UIOne (OneWholeEdit Maybe edit);
        MkUIOneWhole :: forall f edit. (MonadOne f,FullEdit edit) => UISpec edit -> UIOne (OneWholeEdit f edit);
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

    lensFullEdit :: FullEdit edita => GeneralLens edita editb -> ConstraintWitness (FullEdit editb);
    lensFullEdit _ = error "lensFullEdit";
{-
    mapOneEditAspect :: forall f edit. MonadOne f =>
        (forall editb. Edit editb => UISpec editb -> UISpec (OneEdit f editb)) -> Aspect edit -> Aspect (OneEdit f edit);
    mapOneEditAspect ff (MkAspect name uispec lens) = MkAspect name (ff uispec) $ oneLiftGeneralLens lens;
-}
    mapOneWholeEditAspect :: forall f edit. (MonadOne f, FullEdit edit) =>
        (forall editb. FullEdit editb => UISpec editb -> UISpec (OneWholeEdit f editb)) -> Aspect edit -> Aspect (OneWholeEdit f edit);
    mapOneWholeEditAspect ff (MkAspect name uispec lens) = case lensFullEdit lens of
    {
        MkConstraintWitness -> MkAspect name (ff uispec) $ oneWholeLiftGeneralLens lens;
    };
}
