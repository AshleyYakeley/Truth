module Truth.TypeKT.Construct where
{
    import Truth.TypeKT.Info;
    import Truth.TypeKT.Type;
    import Data.Maybe;
    import Control.Monad;

    class (Property f) => Construct f where
    {
        construct :: f t -> Info_W t;
    };

    data Match :: WrappedType -> * where
    {
        MkMatch :: forall f a. Info_W (WrapType f) -> Info_W (WrapType a) -> Match (WrapType (f a));
    };

    instance Property Match where
    {
        matchProperty (MkInfo _ (ConsWit tf ta) _) = return (MkMatch tf ta);
        matchProperty _ = Nothing;
    };

    applyInfo :: Info_W (WrapType f) -> Info_W (WrapType a) -> Info_W (WrapType (f a));
    applyInfo tf@(MkInfo (KindArrow _ kfa) _ inf) ta = MkInfo kfa (ConsWit tf ta) (deriveFacts inf ta);
    applyInfo (MkInfo _ _ _) _ = undefined where
    {
        undefined = undefined;
    };

    instance Construct Match where
    {
        construct (MkMatch tf ta) = applyInfo tf ta;
    };
}
