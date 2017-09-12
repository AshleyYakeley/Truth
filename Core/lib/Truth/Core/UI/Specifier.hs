module Truth.Core.UI.Specifier where
{
    import Truth.Core.Import;


    data Aspect edit where
    {
        MkAspect :: forall edit. String -> UISpec edit -> Aspect edit;
    };

    instance Show (Aspect edit) where
    {
        show (MkAspect name _) = name;
    };

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
}
