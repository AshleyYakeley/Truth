module Data.TypeFunc.TF where
{
    import Prelude();    -- hardcore!

    type family TF tf x;
    
    data TFIdentity;
    type instance TF TFIdentity x = x;
    
    data TFConst a;
    type instance TF (TFConst a) x = a;
    
    data TFApply (f :: * -> *);
    type instance TF (TFApply f) x = f x;
    
    data TFApply1 (f :: * -> * -> *);
    type instance TF (TFApply1 f) x = TFApply (f x);
    
    data TFApply2 (f :: * -> * -> * -> *);
    type instance TF (TFApply2 f) x = TFApply1 (f x);

    data TFLift x y;
    type instance TF (TFLift x y) z = TF (TF x z) (TF y z);
    
    data TFCompose tf1 tf2;
    type instance TF (TFCompose tf1 tf2) x = TF tf1 (TF tf2 x);
    
    data TFConverse tf1 tf2;
    type instance TF (TFConverse tf1 tf2) x = TF (TF tf1 x) tf2;
    
    type TFI = TFIdentity;
    type TFK = TFApply TFConst;
    type TFS = TFApply1 TFLift;
    type TFB = TFApply1 TFCompose;
    type TFC = TFApply1 TFConverse;

    data TFMatch;
    type instance TF TFMatch (f a) = a;

    data TFMatch1;
    type instance TF TFMatch1 (f a b) = a;
    
    data TFMap tf;
    type instance TF (TFMap tf) (f a) = f (TF tf a);
}
