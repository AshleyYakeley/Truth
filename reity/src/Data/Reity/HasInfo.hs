{-# OPTIONS -fno-warn-unused-binds -fno-warn-orphans #-}
module Data.Reity.HasInfo where
{
    import GHC.Types;
    import Data.HasNewValue;
    import Data.Reity.Info;
    import Data.OpenWitness;


    class HasInfo (a :: k) where
    {
        info :: Info a;
    };

    mkSimpleInfo :: forall (k :: *) (t :: k). HasInfo k => IOWitness t -> [ConstraintKnowledge] -> Info t;
    mkSimpleInfo wit facts = MkInfo info $ SimpleWit wit $ mconcat facts;


    -- Type

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


    -- basic types

    instance HasInfo Eq where
    {
        info = mkSimpleInfo $(iowitness[t|Eq|]) [];
    };

    instance HasInfo Functor where
    {
        info = mkSimpleInfo $(iowitness[t|Functor|]) [];
    };

    instance HasInfo Applicative where
    {
        info = mkSimpleInfo $(iowitness[t|Applicative|]) [];
    };

    instance HasInfo Monad where
    {
        info = mkSimpleInfo $(iowitness[t|Monad|]) [];
    };

    instance HasInfo HasNewValue where
    {
        info = mkSimpleInfo $(iowitness[t|HasNewValue|]) [];
    };

    instance HasInfo () where
    {
        info = mkSimpleInfo $(iowitness[t|()|])
        [
            knowConstraint (info :: Info (HasNewValue ())),
            knowConstraint (info :: Info (Eq ()))
        ];
    };

    instance HasInfo Bool where
    {
        info = mkSimpleInfo $(iowitness[t|Bool|])
        [
            knowConstraint (info :: Info (HasNewValue Bool)),
            knowConstraint (info :: Info (Eq Bool))
        ];
    };

    instance HasInfo Char where
    {
        info = mkSimpleInfo $(iowitness[t|Char|])
        [
            knowConstraint (info :: Info (HasNewValue Char)),
            knowConstraint (info :: Info (Eq Char))
        ];
    };

    instance HasInfo Maybe where
    {
        info = mkSimpleInfo $(iowitness[t|Maybe|])
        [
            knowConstraint (info :: Info (Functor Maybe)),
            knowConstraint (info :: Info (Applicative Maybe)),
            knowConstraint (info :: Info (Monad Maybe))
        ];
    };

    instance HasInfo [] where
    {
        info = mkSimpleInfo $(iowitness[t|[]|])
        [
            knowConstraint (info :: Info (Functor Maybe)),
            knowConstraint (info :: Info (Applicative Maybe)),
            knowConstraint (info :: Info (Monad Maybe))
        ];
    };
}
