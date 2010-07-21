{-# OPTIONS -fno-warn-orphans #-}
module Truth.TypeKT.HasType where
{
    import Truth.TypeKT.HasInfo;
    import Truth.TypeKT.Info;
    import Truth.TypeKT.Type;
    import Truth.TypeKT.TH;
    import Control.Monad;
    import Data.HasNewValue;
    import Data.FunctorOne;
    import Data.OpenWitness;
    import Data.Witness;
    import Data.Result;
    import Data.ByteString hiding (concat);
    import Data.Maybe;
    import Data.Word;
    import Data.Bool;
    import Data.Char;
    import Data.Int;
    import Data.Eq;


    data Eq_Inst t where
    {
        Eq_Inst :: forall a. (Eq a) => Eq_Inst (Type_T a);
    };
    $(factInstances [t|Eq_Inst|]);

    data FunctorOne_Inst t where
    {
        FunctorOne_Inst :: forall a. (FunctorOne a) => FunctorOne_Inst (Type_KTT a);
    };
    $(factInstances [t|FunctorOne_Inst|]);

    data HasNewValue_Inst t where
    {
        HasNewValue_Inst :: forall a. (HasNewValue a) => HasNewValue_Inst (Type_T a);
    };
    $(factInstances [t|HasNewValue_Inst|]);





    instance HasInfo (Type_T ()) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T () |])
        [
            mkFacts (MkFactZ (do
            {
                return HasNewValue_Inst;
            })
            :: FactZ HasNewValue_Inst (Type_T ())
            ),
            mkFacts (MkFactZ (do
            {
                return Eq_Inst;
            })
            :: FactZ Eq_Inst (Type_T ())
            )
        ];
    };

    instance HasInfo (Type_KTT Maybe) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KTT Maybe |])
        [
            -- instance () => HasNewValue (Maybe a)
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                Kind_T <- matchProp $(type1[t|Kind_T|]) a;
                Kind_T <- matchProp $(type1[t|Kind_T|]) a;
                return HasNewValue_Inst;
            }))
            :: FactS FactZ HasNewValue_Inst (Type_KTT Maybe)
            ),

            -- instance (Eq a) => Eq (Maybe a)
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                Eq_Inst <- matchProp $(type1[t|Eq_Inst|]) a;
                return Eq_Inst;
            }))
            :: FactS FactZ Eq_Inst (Type_KTT Maybe)
            ),

            -- instance FunctorOne Maybe
            mkFacts (MkFactZ (do
            {
                return FunctorOne_Inst;
            })
            :: FactZ FunctorOne_Inst (Type_KTT Maybe)
            )
        ];
    };

    instance HasInfo (Type_KTT []) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KTT [] |])
        [
            -- instance () => HasNewValue ([] a)
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                Kind_T <- matchProp $(type1[t|Kind_T|]) a;
                return HasNewValue_Inst;
            }))
            :: FactS FactZ HasNewValue_Inst (Type_KTT [])
            ),

            -- instance (Eq a) => Eq ([] a)
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                Eq_Inst <- matchProp $(type1[t|Eq_Inst|]) a;
                return Eq_Inst;
            }))
            :: FactS FactZ Eq_Inst (Type_KTT [])
            )
        ];
    };







    -- T
{-
    instance HasInfo (Type_T ()) where
    {
        info = MkInfo
            Kind_T
            (SimpleWit $(iowitness[t| Type_T () |]))
            (
                (mkFacts (return HasNewValue_Inst)) `mappend`
                (mkFacts (return Eq_Inst))
            );
    };
-}
    instance HasInfo (Type_T Bool) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T Bool |])
        [
            mkFacts0 (return HasNewValue_Inst),
            mkFacts0 (return Eq_Inst)
        ];
    };

    instance HasInfo (Type_T Word8) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T Word8 |])
        [
            mkFacts0 (return HasNewValue_Inst),
            mkFacts0 (return Eq_Inst)
        ];
    };

    instance HasInfo (Type_T Char) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T Char |])
        [
            mkFacts0 (return HasNewValue_Inst),
            mkFacts0 (return Eq_Inst)
        ];
    };

    instance HasInfo (Type_T Int) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T Int |])
        [
            mkFacts0 (return HasNewValue_Inst),
            mkFacts0 (return Eq_Inst)
        ];
    };

    instance HasInfo (Type_T ByteString) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T ByteString |])
        [
            mkFacts0 (return HasNewValue_Inst),
            mkFacts0 (return Eq_Inst)
        ];
    };


    -- KTT
{-
    instance (HasInfo (Type_KTT f),HasInfo (Type_T a)) => HasInfo (Type_T (f a)) where
    {
        info = applyInfo info info;
    };

    instance HasInfo (Type_KTT Maybe) where
    {
        info = MkInfo
            (SimpleWit $(iowitness[t| Type_KTT Maybe |]))
            (mconcat
            [
                mkFacts (\_ -> return HasNewValue_Inst),
                mkFacts (\t -> do
                {
                    Eq_Inst <- matchProperty t;
                    return Eq_Inst;
                }),
                mkFacts (return FunctorOne_Inst)
            ]);
    };

    instance HasInfo (Type_KTT []) where
    {
        info = MkInfo
            (SimpleWit $(iowitness[t| Type_KTT [] |]))
            (mconcat
            [
                mkFacts (\_ -> return HasNewValue_Inst),
                mkFacts (\t -> do
                {
                    Eq_Inst <- matchProperty t;
                    return Eq_Inst;
                })
            ]);
    };
-}

    -- KTKTT

    instance HasInfo (Type_KTKTT (->)) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KTKTT (->) |])
        [
        ];
    };

    instance HasInfo (Type_KTKTT Result) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KTKTT Result |])
        [
            -- instance (HasNewValue a1) => HasNewValue (Result a0 a1)
            mkFacts (MkFactS (\a0 -> MkFactS (\a1 -> MkFactZ (do
            {
                Kind_T <- matchProp $(type1[t|Kind_T|]) a0;
                Kind_T <- matchProp $(type1[t|Kind_T|]) a1;
                HasNewValue_Inst <- matchProp $(type1[t|HasNewValue_Inst|]) a1;
                return HasNewValue_Inst;
            })))
            :: FactS (FactS FactZ) HasNewValue_Inst (Type_KTKTT Result)
            ),

            -- mkFacts1 (\_ -> return FunctorOne_Inst)
            mkFacts (MkFactS (\a0 -> MkFactZ (do
            {
                Kind_T <- matchProp $(type1[t|Kind_T|]) a0;
                return FunctorOne_Inst;
            }))
            :: FactS FactZ FunctorOne_Inst (Type_KTKTT Result)
            )
        ];
    };


    -- KKTTT

    instance HasInfo (Type_KKTTT Any) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KKTTT Any |])
        [
        ];
    };
}
