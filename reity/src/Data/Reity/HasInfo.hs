module Data.Reity.HasInfo where
{
    import GHC.Types;
    import Data.Word;
    import Data.ByteString;
    import Data.Type.Heterogeneous;
    import Data.HasNewValue;
    import Data.MonadOne;
    import Data.Result;
    import Data.OpenWitness;
    import Data.Knowledge;
    import Data.Reity.Info;
    import Data.Reity.Match;


    class HasInfo a where
    {
        info :: Info a;
    };

    isInfo :: forall a b. HasInfo a => Info b -> Maybe (HetEq a b);
    isInfo = testHetEquality info;

    mkSimpleInfo :: forall (k :: *) (t :: k). HasInfo k => IOWitness t -> [TypeKnowledge] -> Info t;
    mkSimpleInfo wit facts = MkInfo info $ SimpleWit wit $ mconcat facts;

    knowC :: forall (c :: Constraint). (c,HasInfo c) => TypeKnowledge;
    knowC = knowConstraint $ info @c;


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

    instance HasInfo MonadOne where
    {
        info = mkSimpleInfo $(iowitness[t|MonadOne|]) [];
    };

    instance HasInfo () where
    {
        info = mkSimpleInfo $(iowitness[t|()|])
        [
            knowC @(HasNewValue ()),
            knowC @(Eq ())
        ];
    };

    instance HasInfo Bool where
    {
        info = mkSimpleInfo $(iowitness[t|Bool|])
        [
            knowC @(HasNewValue Bool),
            knowC @(Eq Bool)
        ];
    };

    instance HasInfo Char where
    {
        info = mkSimpleInfo $(iowitness[t|Char|])
        [
            knowC @(HasNewValue Char),
            knowC @(Eq Char)
        ];
    };

    instance HasInfo Word8 where
    {
        info = mkSimpleInfo $(iowitness[t|Word8|])
        [
            knowC @(HasNewValue Word8),
            knowC @(Eq Word8)
        ];
    };

    instance HasInfo Int where
    {
        info = mkSimpleInfo $(iowitness[t|Int|])
        [
            knowC @(HasNewValue Int),
            knowC @(Eq Int)
        ];
    };

    instance HasInfo ByteString where
    {
        info = mkSimpleInfo $(iowitness[t|ByteString|])
        [
            knowC @(HasNewValue ByteString),
            knowC @(Eq ByteString)
        ];
    };

    instance HasInfo Maybe where
    {
        info = mkSimpleInfo $(iowitness[t|Maybe|])
        [
            knowC @(Functor Maybe),
            knowC @(Applicative Maybe),
            knowC @(Monad Maybe)
            -- instance HasNewValue (Maybe a)
            -- instance (Eq a) => Eq (Maybe a)
            -- instance MonadOne Maybe
    ];
    };

    instance HasInfo [] where
    {
        info = mkSimpleInfo $(iowitness[t|[]|])
        [
            knowC @(Functor []),
            knowC @(Applicative []),
            knowC @(Monad [])
            -- instance () => HasNewValue ([] a)
            -- instance (Eq a) => Eq ([] a)
        ];
    };

    instance HasInfo Either where
    {
        info = mkSimpleInfo $(iowitness[t|Either|])
        [
        ];
    };

    instance HasInfo Result where
    {
        info = mkSimpleInfo $(iowitness[t|Result|])
        [
            -- instance HasNewValue a => HasNewValue (Result e a)
            MkKnowledge $ \knowledge hreaInfo -> do
            {
                MkSplitInfo hInfo reaInfo <- matchInfo hreaInfo;
                ReflH <- sameInfo (info @HasNewValue) hInfo;
                MkSplitInfo reInfo aInfo <- matchInfo reaInfo;
                MkSplitInfo rInfo _ <- matchInfo reInfo;
                ReflH <- sameInfo (info @Result) rInfo;
                ConstraintFact <- ask knowledge $ applyInfo (info @HasNewValue) aInfo;
                return ConstraintFact;
            }

            -- instance Functor
            -- instance MonadOne
        ];
    };

{-
    instance HasInfo Any where
    {
        info = mkSimpleInfo $(iowitness[t|Any|]) [];
    };
-}
}
