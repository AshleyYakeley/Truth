{-# OPTIONS -fno-warn-orphans #-}
module Data.Reity.Instances where
{
    import GHC.Types;
    import Data.Type.Equality;
    import Data.Type.Heterogeneous;
    import Data.Word;
    import Data.ByteString.Lazy;
    import Control.Monad.IO.Class;
    import Control.Comonad;
    import Data.UUID;
    import Data.Searchable;
    import Data.HasNewValue;
    import Data.MonadOne;
    import Data.Result;
    import Data.Knowledge;
    import Data.Reity.ReasonM;
    import Data.Reity.Match;
    import Data.Reity.Wit;
    import Data.Reity.TypeInfo;
    import Data.Reity.Template;


    -- Type

    instance HasTypeInfo TYPE where
    {
        typeWitness = $(generateWitness [t|TYPE|]);
        typeName _ = "TYPE";
    };

    type ARR = (->);

    instance HasTypeInfo (->) where
    {
        typeWitness = $(generateWitness [t|ARR|]);
        typeName _ = "(->)";
    };

    instance HasTypeInfo Constraint where
    {
        typeWitness = $(generateWitness [t|Constraint|]);
        typeName _ = "Constraint";
    };

    instance HasTypeInfo RuntimeRep where
    {
        typeWitness = $(generateWitness [t|RuntimeRep|]);
        typeName _ = "RuntimeRep";
    };

    instance HasTypeInfo 'PtrRepLifted where
    {
        typeWitness = $(generateWitness [t|'PtrRepLifted|]);
        typeName _ = "'PtrRepLifted";
    };


    -- some classes

    instance HasTypeInfo k => HasTypeInfo (HasTypeInfo :: k -> Constraint) where
    {
        typeWitness = $(generateWitness [t|HasTypeInfo|]);
        typeName _ = "HasTypeInfo";
        typeKnowledge _ = baseTypeKnowledge;
    };

    baseTypeKnowledge :: TypeKnowledge;
    baseTypeKnowledge = MkKnowledge $ \ihtia -> do
    {
        -- tie the HasTypeInfo knot
        MkSplitTypeInfo ihti ia <- matchTypeInfo ihtia;
        (MkTypeInfo :: TypeInfo k) <- return $ typeInfoKind ia;
        ReflH <- sameTypeInfo ihti (typeInfo @(HasTypeInfo :: k -> Constraint));
        MkTypeInfo <- return ia;
        return ConstraintFact;
    };

    instance HasTypeInfo Eq where
    {
        typeWitness = $(generateWitness [t|Eq|]);
        typeName _ = "Eq";
    };

    instance HasTypeInfo Ord where
    {
        typeWitness = $(generateWitness [t|Ord|]);
        typeName _ = "Ord";
    };

    instance HasTypeInfo Enum where
    {
        typeWitness = $(generateWitness [t|Enum|]);
        typeName _ = "Enum";
    };

    instance HasTypeInfo Num where
    {
        typeWitness = $(generateWitness [t|Num|]);
        typeName _ = "Num";
    };

    instance HasTypeInfo Integral where
    {
        typeWitness = $(generateWitness [t|Integral|]);
        typeName _ = "Integral";
    };

    instance HasTypeInfo Real where
    {
        typeWitness = $(generateWitness [t|Real|]);
        typeName _ = "Real";
    };

    instance HasTypeInfo Show where
    {
        typeWitness = $(generateWitness [t|Show|]);
        typeName _ = "Show";
    };

    instance HasTypeInfo Read where
    {
        typeWitness = $(generateWitness [t|Read|]);
        typeName _ = "Read";
    };

    instance HasTypeInfo k => HasTypeInfo (TestEquality :: (k -> *) -> Constraint) where
    {
        typeWitness = $(generateWitness [t|TestEquality|]);
        typeName _ = "TestEquality";
    };

    instance HasTypeInfo Monoid where
    {
        typeWitness = $(generateWitness [t|Monoid|]);
        typeName _ = "Monoid";
    };

    instance HasTypeInfo Foldable where
    {
        typeWitness = $(generateWitness [t|Foldable|]);
        typeName _ = "Foldable";
    };

    instance HasTypeInfo Traversable where
    {
        typeWitness = $(generateWitness [t|Traversable|]);
        typeName _ = "Traversable";
    };

    instance HasTypeInfo Functor where
    {
        typeWitness = $(generateWitness [t|Functor|]);
        typeName _ = "Functor";
    };

    instance HasTypeInfo Applicative where
    {
        typeWitness = $(generateWitness [t|Applicative|]);
        typeName _ = "Applicative";
    };

    instance HasTypeInfo Monad where
    {
        typeWitness = $(generateWitness [t|Monad|]);
        typeName _ = "Monad";
    };

    instance HasTypeInfo MonadIO where
    {
        typeWitness = $(generateWitness [t|MonadIO|]);
        typeName _ = "MonadIO";
    };

    instance HasTypeInfo Comonad where
    {
        typeWitness = $(generateWitness [t|Comonad|]);
        typeName _ = "Comonad";
    };

    instance HasTypeInfo HasNewValue where
    {
        typeWitness = $(generateWitness [t|HasNewValue|]);
        typeName _ = "HasNewValue";
    };

    instance HasTypeInfo MonadOne where
    {
        typeWitness = $(generateWitness [t|MonadOne|]);
        typeName _ = "MonadOne";
    };

    instance HasTypeInfo Finite where
    {
        typeWitness = $(generateWitness [t|Finite|]);
        typeName _ = "Finite";
    };


    -- basic types

    instance HasTypeInfo () where
    {
        typeWitness = $(generateWitness [t|()|]);
        typeName _ = "()";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance HasNewValue ();
            instance Monoid ();
            instance Eq ();
        |]);
    };

    instance HasTypeInfo Bool where
    {
        typeWitness = $(generateWitness [t|Bool|]);
        typeName _ = "Bool";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance HasNewValue Bool;
            instance Eq Bool;
        |]);
    };

    instance HasTypeInfo Char where
    {
        typeWitness = $(generateWitness [t|Char|]);
        typeName _ = "Char";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance HasNewValue Char;
            instance Eq Char;
            instance Ord Char;
        |]);
    };

    instance HasTypeInfo Word8 where
    {
        typeWitness = $(generateWitness [t|Word8|]);
        typeName _ = "Word8";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance HasNewValue Word8;
            instance Eq Word8;
            instance Ord Word8;
        |]);
    };

    instance HasTypeInfo Int where
    {
        typeWitness = $(generateWitness [t|Int|]);
        typeName _ = "Int";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance HasNewValue Int;
            instance Eq Int;
            instance Ord Int;
        |]);
    };

    instance HasTypeInfo ByteString where
    {
        typeWitness = $(generateWitness [t|ByteString|]);
        typeName _ = "ByteString";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance HasNewValue ByteString;
            instance Monoid ByteString;
            instance Eq ByteString;
        |]);
    };

    instance HasTypeInfo Maybe where
    {
        typeWitness = $(generateWitness [t|Maybe|]);
        typeName _ = "Maybe";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Foldable Maybe;
            instance Traversable Maybe;
            instance Functor Maybe;
            instance Applicative Maybe;
            instance Monad Maybe;
            instance MonadOne Maybe;
            instance HasNewValue (Maybe a);
            instance (Eq a) => Eq (Maybe a);
            instance MonadOne Maybe;
        |]);
    };

    instance HasTypeInfo [] where
    {
        typeWitness = $(generateWitness [t|[]|]);
        typeName _ = "[]";
        typeNameApply _ s = "[" ++ s ++ "]";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Foldable [];
            instance Traversable [];
            instance Functor [];
            instance Applicative [];
            instance Monad [];
            instance () => HasNewValue ([] a);
            instance (Eq a) => Eq ([] a);
        |]);
    };

    instance HasTypeInfo (,) where
    {
        typeWitness = $(generateWitness [t|(,)|]);
        typeName _ = "(,)";
        typeKnowledge _ = $(generateTypeKnowledge [d|
        |]);
    };

    instance HasTypeInfo Either where
    {
        typeWitness = $(generateWitness [t|Either|]);
        typeName _ = "Either";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Foldable (Either a);
            instance Traversable (Either a);
            instance Functor (Either a);
            instance Applicative (Either a);
            instance Monad (Either a);
            instance MonadOne (Either a);
        |]);
    };

    instance HasTypeInfo Result where
    {
        typeWitness = $(generateWitness [t|Result|]);
        typeName _ = "Result";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance HasNewValue a => HasNewValue (Result e a);
            instance Foldable (Result e);
            instance Traversable (Result e);
            instance Functor (Result e);
            instance Applicative (Result e);
            instance Monad (Result e);
            instance MonadOne (Result e);
        |]);
    };

    instance HasTypeInfo FailureReasons where
    {
        typeWitness = $(generateWitness [t|FailureReasons|]);
        typeName _ = "FailureReasons";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Eq FailureReasons;
            instance Monoid FailureReasons;
        |]);
    };

    instance HasTypeInfo UUID where
    {
        typeWitness = $(generateWitness [t|UUID|]);
        typeName _ = "UUID";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Eq UUID;
            instance Ord UUID;
            instance Show UUID;
        |]);
    };
}
