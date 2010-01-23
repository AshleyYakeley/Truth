module Truth.TypeKT.Witness where
{
    import Truth.TypeKT.Basic;
    import Data.Witness;
    import Data.Maybe;

    -- T = *
    type EqualTypeT = EqualType;

    class (SimpleWitness w) => WitnessT w where
    {
        matchWitnessT :: forall a b. w a -> w b -> Maybe (EqualTypeT a b);
    };

    instance (SimpleWitness w) => WitnessT w where
    {
        matchWitnessT = matchWitness;
    };


    -- KTT = * -> *
    type EqualTypeKTT a b = EqualTypeT (a FT) (b FT);

    class WitnessKTT w where
    {
        matchWitnessKTT :: forall a b. w a -> w b -> Maybe (EqualTypeKTT a b);
    };


    -- KKTTT = (* -> *) -> *
    type EqualTypeKKTTT a b = EqualTypeT (a FKTT) (b FKTT);

    class WitnessKKTTT w where
    {
        matchWitnessKKTTT :: forall a b. w a -> w b -> Maybe (EqualTypeKKTTT a b);
    };


    -- KTKTT = * -> * -> *
    type EqualTypeKTKTT a b = EqualTypeKTT (a FT) (b FT);

    class WitnessKTKTT w where
    {
        matchWitnessKTKTT :: forall a b. w a -> w b -> Maybe (EqualTypeKTKTT a b);
    };


    -- KKTTKTT = (* -> *) -> * -> *
    type EqualTypeKKTTKTT a b = EqualTypeKTT (a FKTT) (b FKTT);

    class WitnessKKTTKTT w where
    {
        matchWitnessKKTTKTT :: forall a b. w a -> w b -> Maybe (EqualTypeKKTTKTT a b);
    };


    -- KTKTKTT = * -> * -> * -> *
    type EqualTypeKTKTKTT a b = EqualTypeKTKTT (a FT) (b FT);

    class WitnessKTKTKTT w where
    {
        matchWitnessKTKTKTT :: forall a b. w a -> w b -> Maybe (EqualTypeKTKTKTT a b);
    };

}
