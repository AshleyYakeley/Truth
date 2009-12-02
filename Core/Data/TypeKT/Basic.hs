module Data.TypeKT.Basic where
{
    data FT;
    data FKTT (a0 :: *);
    data FKKTTT (a0 :: * -> *);
    data FKTKTT (a0 :: *) (a1 :: *);
    data FKTKTKTT (a0 :: *) (a1 :: *) (a2 :: *);
    data FKKTTKTT (a0 :: * -> *) (a1 :: *);

    type SatT a = a;
    type SatKTT a = a FT;
    type SatKTKTT a = a FT FT;
    type SatKKTTT a = a FKTT;
    type SatKKTTKTT a = a FKTT FT;
    type SatKTKTKTT a = a FT FT FT;
    type SatKTKTKTKTT a = a FT FT FT FT;
    type SatKTKKTTKTT a = a FT FKTT FT;
    type SatKKTKTTT a = a FKTKTT;
    type SatKKKTTTT a = a FKKTTT;
    type SatKKTKTKTTT a = a FKTKTKTT;
    type SatKKKTTKTTT a = a FKKTTKTT;
}
