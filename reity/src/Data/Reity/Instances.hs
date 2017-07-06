{-# OPTIONS -fno-warn-orphans #-}
module Data.Reity.Instances where
{
    import GHC.Types;
    import Data.Type.Equality;
    import Data.Word;
    import Data.ByteString;
    import Control.Comonad;
    import Data.UUID;
    import Data.Searchable;
    import Data.HasNewValue;
    import Data.MonadOne;
    import Data.Result;
    import Data.OpenWitness;
    import Data.Reity.Match;
    import Data.Reity.HasInfo;
    import Data.Reity.Template;


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

    instance (HasInfo f,HasInfo a) => HasInfo (f a) where
    {
        info = applyInfo info info;
    };


    -- some classes

    instance HasInfo Eq where
    {
        info = mkSimpleInfo $(iowitness[t|Eq|]) [];
    };

    instance HasInfo Ord where
    {
        info = mkSimpleInfo $(iowitness[t|Ord|]) [];
    };

    instance HasInfo Enum where
    {
        info = mkSimpleInfo $(iowitness[t|Enum|]) [];
    };

    instance HasInfo Num where
    {
        info = mkSimpleInfo $(iowitness[t|Num|]) [];
    };

    instance HasInfo Integral where
    {
        info = mkSimpleInfo $(iowitness[t|Integral|]) [];
    };

    instance HasInfo Real where
    {
        info = mkSimpleInfo $(iowitness[t|Real|]) [];
    };

    instance HasInfo Show where
    {
        info = mkSimpleInfo $(iowitness[t|Show|]) [];
    };

    instance HasInfo Read where
    {
        info = mkSimpleInfo $(iowitness[t|Read|]) [];
    };

    instance HasInfo k => HasInfo (TestEquality :: (k -> *) -> Constraint) where
    {
        info = mkSimpleInfo $(iowitness[t|TestEquality|]) [];
    };

    instance HasInfo Monoid where
    {
        info = mkSimpleInfo $(iowitness[t|Monoid|]) [];
    };

    instance HasInfo Foldable where
    {
        info = mkSimpleInfo $(iowitness[t|Foldable|]) [];
    };

    instance HasInfo Traversable where
    {
        info = mkSimpleInfo $(iowitness[t|Traversable|]) [];
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

    instance HasInfo Comonad where
    {
        info = mkSimpleInfo $(iowitness[t|Comonad|]) [];
    };

    instance HasInfo HasNewValue where
    {
        info = mkSimpleInfo $(iowitness[t|HasNewValue|]) [];
    };

    instance HasInfo MonadOne where
    {
        info = mkSimpleInfo $(iowitness[t|MonadOne|]) [];
    };

    instance HasInfo Finite where
    {
        info = mkSimpleInfo $(iowitness[t|Finite|]) [];
    };


    -- basic types

    instance HasInfo () where
    {
        info = mkSimpleInfo $(iowitness[t|()|]) [$(declInfo [d|
            instance HasNewValue ();
            instance Monoid ();
            instance Eq ();
        |])];
    };

    instance HasInfo Bool where
    {
        info = mkSimpleInfo $(iowitness[t|Bool|]) [$(declInfo [d|
            instance HasNewValue Bool;
            instance Eq Bool;
        |])];
    };

    instance HasInfo Char where
    {
        info = mkSimpleInfo $(iowitness[t|Char|]) [$(declInfo [d|
            instance HasNewValue Char;
            instance Eq Char;
            instance Ord Char;
        |])];
    };

    instance HasInfo Word8 where
    {
        info = mkSimpleInfo $(iowitness[t|Word8|]) [$(declInfo [d|
            instance HasNewValue Word8;
            instance Eq Word8;
            instance Ord Word8;
        |])];
    };

    instance HasInfo Int where
    {
        info = mkSimpleInfo $(iowitness[t|Int|]) [$(declInfo [d|
            instance HasNewValue Int;
            instance Eq Int;
            instance Ord Int;
        |])];
    };

    instance HasInfo ByteString where
    {
        info = mkSimpleInfo $(iowitness[t|ByteString|]) [$(declInfo [d|
            instance HasNewValue ByteString;
            instance Monoid ByteString;
            instance Eq ByteString;
        |])];
    };

    instance HasInfo Maybe where
    {
        info = mkSimpleInfo $(iowitness[t|Maybe|]) [$(declInfo [d|
            instance Foldable Maybe;
            instance Traversable Maybe;
            instance Functor Maybe;
            instance Applicative Maybe;
            instance Monad Maybe;
            instance MonadOne Maybe;
            instance HasNewValue (Maybe a);
            instance (Eq a) => Eq (Maybe a);
            instance MonadOne Maybe;
        |])];
    };

    instance HasInfo [] where
    {
        info = mkSimpleInfo $(iowitness[t|[]|]) [$(declInfo [d|
            instance Foldable [];
            instance Traversable [];
            instance Functor [];
            instance Applicative [];
            instance Monad [];
            instance () => HasNewValue ([] a);
            instance (Eq a) => Eq ([] a);
        |])];
    };

    instance HasInfo (,) where
    {
        info = mkSimpleInfo $(iowitness[t|(,)|]) [$(declInfo [d|
        |])];
    };

    instance HasInfo Either where
    {
        info = mkSimpleInfo $(iowitness[t|Either|]) [$(declInfo [d|
            instance Foldable (Either a);
            instance Traversable (Either a);
            instance Functor (Either a);
            instance Applicative (Either a);
            instance Monad (Either a);
            instance MonadOne (Either a);
        |])];
    };

    instance HasInfo Result where
    {
        info = mkSimpleInfo $(iowitness[t|Result|]) [$(declInfo [d|
            instance HasNewValue a => HasNewValue (Result e a);
            instance Foldable (Result e);
            instance Traversable (Result e);
            instance Functor (Result e);
            instance Applicative (Result e);
            instance Monad (Result e);
            instance MonadOne (Result e);
        |])];
    };

    instance HasInfo UUID where
    {
        info = mkSimpleInfo $(iowitness[t|UUID|]) [$(declInfo [d|
            instance Eq UUID;
            instance Ord UUID;
        |])];
    };

{-
    instance HasInfo Any where
    {
        info = mkSimpleInfo $(iowitness[t|Any|]) [];
    };
-}
}
