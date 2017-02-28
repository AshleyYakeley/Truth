{-# OPTIONS -fno-warn-unused-binds -fno-warn-orphans #-}
module Data.Reity.HasInfo where
{
    import Data.Compose;
    import Data.KindCategory;

--    import Data.Reity.Construct;
    import Data.Reity.Info;
    import Data.OpenWitness;
--    import Data.Witness;
--    import Data.List;
--    import Data.Maybe;
--    import Data.Bool;
--    import Data.Int;
--    import Data.Eq;
--    import Control.Monad;
    -- import Prelude(Num(..),fromInteger);

    import GHC.Types;

    class HasInfo (a :: k) where
    {
        info :: Info a;
    };


    mkSimpleInfo :: forall (k :: *) (t :: k). HasInfo k => IOWitness t -> [Facts t] -> Info t;
    mkSimpleInfo wit facts = MkInfo info (SimpleWit wit) (mconcat facts);


    instance HasInfo TYPE where
    {
        info = mkSimpleInfo $(iowitness[t|TYPE|]) [];
    };

    type ARR = (->);

    instance HasInfo (->) where
    {
        info = mkSimpleInfo $(iowitness[t|ARR|]) [];
    };

    instance HasInfo Constraint where
    {
        info = mkSimpleInfo $(iowitness[t|Constraint|]) [];
    };

    instance HasInfo RuntimeRep where
    {
        info = mkSimpleInfo $(iowitness[t|RuntimeRep|]) [];
    };

    instance HasInfo 'PtrRepLifted where
    {
        info = mkSimpleInfo $(iowitness[t|'PtrRepLifted|]) [];
    };

    instance (HasInfo f,HasInfo a) => HasInfo ((f :: kp -> kq) (a :: kp)) where
    {
        info = applyInfo info info;
    };

{-
    instance (HasInfo a) => Property (EqualType t) where
    {
        matchProperty = testEquality info;
    };
-}

    type ConstraintFact (cons :: k -> Constraint) = (Compose ConstraintWitness cons :: k -> *);
}
