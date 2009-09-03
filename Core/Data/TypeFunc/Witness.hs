module Data.TypeFunc.Witness where
{
    import Data.TypeFunc.TF;
    import Data.Chain;
    import Data.Witness;
    import Control.Category;

    data TFWitness w x y where
    {
        MkTFWitness :: w tf -> TFWitness w x (TF tf x);
    };
    
    class CatFunctorWitness w where
    {
        tfMap :: w a -> w (TFMap a);
    };
    
    instance (CatFunctorWitness w) => CatFunctor (TFWitness w) f where
    {
        cfmap (MkTFWitness wtf) = MkTFWitness (tfMap wtf);
    };
    
    class CategoryWitness w where
    {
        tfIdentity :: w TFIdentity;
        tfCompose :: w bc -> w ab -> w (TFCompose bc ab);
    };
    
    instance CategoryWitness w => Category (TFWitness w) where
    {
        id = MkTFWitness tfIdentity;
        (MkTFWitness bc) . (MkTFWitness ab) = MkTFWitness (tfCompose bc ab);
    };
    
    instance (SimpleWitness w) => SimpleWitness1 (TFWitness w) where
    {
        matchWitness1 (MkTFWitness wtf1) (MkTFWitness wtf2) = do
        {
            MkEqualType <- matchWitness wtf1 wtf2;
            return MkEqualType;
        };
    };

    matchTWitness :: (SimpleWitness w) => Type p -> TFWitness w p a -> TFWitness w p b -> Maybe (EqualType a b);
    matchTWitness _ = matchWitness;

    data TFMappable w tf where
    {
        SimpleTFMappable :: w tf -> TFMappable w tf;
        MappedTFMappable :: TFMappable w tf -> TFMappable w (TFMap tf);
    };
    
    instance (SimpleWitness w) => SimpleWitness (TFMappable w) where
    {
        matchWitness (SimpleTFMappable w1) (SimpleTFMappable w2) = matchWitness w1 w2;
        matchWitness (MappedTFMappable w1) (MappedTFMappable w2) = do
        {
            MkEqualType <- matchWitness w1 w2;
            return MkEqualType;
        };
        matchWitness _ _ = Nothing;
    };
    
    instance CatFunctorWitness (TFMappable w) where
    {
        tfMap = MappedTFMappable;
    };

{-    not decidable
    data TFComposite w tf where
    {
        ConstTFComposite :: w tf -> TFComposite w tf;
        ApplyTFComposite :: TFComposite w tf1 -> TFComposite w tf2 -> TFComposite w (TF tf1 tf2);
    };
    
    instance (SimpleWitness w) => SimpleWitness (TFComposite w) where
    {
        matchWitness (ConstTFComposite w1) (ConstTFComposite w2) = matchWitness w1 w2;
        matchWitness (ApplyTFComposite a1 a2) (ApplyTFComposite b1 b2) = do
        {
            MkEqualType <- matchWitness a1 b1;
            MkEqualType <- matchWitness a2 b2;
            return MkEqualType;
        };
        matchWitness _ _ = Nothing;
    };
-}
}
