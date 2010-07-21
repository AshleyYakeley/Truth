{-# LANGUAGE CPP #-}
module Truth.TypeKT.Construct where
{
    import Truth.TypeKT.Info;
    import Truth.TypeKT.Type;
    import Data.Maybe;
    import Control.Monad;

    class (Property f) => Construct f where
    {
        construct :: f t -> Info t;
    };

    data Match t where
    {
        MkMatch :: forall f a. (ConstructType f a) => Info f -> Info a -> Match (TypeConstructed f a);
    };

    instance Property Match where
    {
        matchProperty (MkInfo (ConsWit tf ta) _) = return (MkMatch tf ta);
        matchProperty _ = Nothing;
    };

    applyInfo :: (ConstructType f a) => Info f -> Info a -> Info (TypeConstructed f a);
    applyInfo tf@(MkInfo _ inf) ta@(MkInfo _ _) = MkInfo (ConsWit tf ta) (deriveFacts inf ta);

    instance Construct Match where
    {
        construct (MkMatch tf ta) = applyInfo tf ta;
    };
}
