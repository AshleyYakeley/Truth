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
    import Data.Reity.Match;
    import Data.Reity.HasInfo;
    import Data.Reity.Template;


    -- Type

    instance HasInfo TYPE where
    {
        info = mkSimpleInfo $(ionamedwitness[t|TYPE|]) [];
    };

    type ARR = (->);

    instance HasInfo (->) where
    {
        info = mkSimpleInfo $(ionamedwitness[t|ARR|]) [];
    };

    instance HasInfo Constraint where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Constraint|]) [];
    };

    instance HasInfo RuntimeRep where
    {
        info = mkSimpleInfo $(ionamedwitness[t|RuntimeRep|]) [];
    };

    instance HasInfo 'PtrRepLifted where
    {
        info = mkSimpleInfo $(ionamedwitness[t|'PtrRepLifted|]) [];
    };

    instance (HasInfo f,HasInfo a) => HasInfo (f a) where
    {
        info = applyInfo info info;
    };


    -- some classes

    instance HasInfo Eq where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Eq|]) [];
    };

    instance HasInfo Ord where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Ord|]) [];
    };

    instance HasInfo Enum where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Enum|]) [];
    };

    instance HasInfo Num where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Num|]) [];
    };

    instance HasInfo Integral where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Integral|]) [];
    };

    instance HasInfo Real where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Real|]) [];
    };

    instance HasInfo Show where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Show|]) [];
    };

    instance HasInfo Read where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Read|]) [];
    };

    instance HasInfo k => HasInfo (TestEquality :: (k -> *) -> Constraint) where
    {
        info = mkSimpleInfo $(ionamedwitness[t|TestEquality|]) [];
    };

    instance HasInfo Monoid where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Monoid|]) [];
    };

    instance HasInfo Foldable where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Foldable|]) [];
    };

    instance HasInfo Traversable where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Traversable|]) [];
    };

    instance HasInfo Functor where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Functor|]) [];
    };

    instance HasInfo Applicative where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Applicative|]) [];
    };

    instance HasInfo Monad where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Monad|]) [];
    };

    instance HasInfo Comonad where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Comonad|]) [];
    };

    instance HasInfo HasNewValue where
    {
        info = mkSimpleInfo $(ionamedwitness[t|HasNewValue|]) [];
    };

    instance HasInfo MonadOne where
    {
        info = mkSimpleInfo $(ionamedwitness[t|MonadOne|]) [];
    };

    instance HasInfo Finite where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Finite|]) [];
    };


    -- basic types

    instance HasInfo () where
    {
        info = mkSimpleInfo $(ionamedwitness[t|()|]) [$(declInfo [d|
            instance HasNewValue ();
            instance Monoid ();
            instance Eq ();
        |])];
    };

    instance HasInfo Bool where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Bool|]) [$(declInfo [d|
            instance HasNewValue Bool;
            instance Eq Bool;
        |])];
    };

    instance HasInfo Char where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Char|]) [$(declInfo [d|
            instance HasNewValue Char;
            instance Eq Char;
            instance Ord Char;
        |])];
    };

    instance HasInfo Word8 where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Word8|]) [$(declInfo [d|
            instance HasNewValue Word8;
            instance Eq Word8;
            instance Ord Word8;
        |])];
    };

    instance HasInfo Int where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Int|]) [$(declInfo [d|
            instance HasNewValue Int;
            instance Eq Int;
            instance Ord Int;
        |])];
    };

    instance HasInfo ByteString where
    {
        info = mkSimpleInfo $(ionamedwitness[t|ByteString|]) [$(declInfo [d|
            instance HasNewValue ByteString;
            instance Monoid ByteString;
            instance Eq ByteString;
        |])];
    };

    instance HasInfo Maybe where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Maybe|]) [$(declInfo [d|
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
        info = mkSimpleInfo $(ionamedwitness[t|[]|]) [$(declInfo [d|
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
        info = mkSimpleInfo $(ionamedwitness[t|(,)|]) [$(declInfo [d|
        |])];
    };

    instance HasInfo Either where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Either|]) [$(declInfo [d|
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
        info = mkSimpleInfo $(ionamedwitness[t|Result|]) [$(declInfo [d|
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
        info = mkSimpleInfo $(ionamedwitness[t|UUID|]) [$(declInfo [d|
            instance Eq UUID;
            instance Ord UUID;
            instance Show UUID;
        |])];
    };

{-
    instance HasInfo Any where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Any|]) [];
    };
-}
}
