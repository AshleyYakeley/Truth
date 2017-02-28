{-# OPTIONS -fno-warn-orphans #-}
module Data.Reity.HasType where
{
    import Data.Reity.HasInfo;
    import Data.Reity.Construct;
    import Data.Reity.Info;
    import Data.Reity.Type;
--    import Data.Reity.TH;
    import Data.KindCategory;
    import Data.Compose;
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
    import GHC.Exts hiding (Any);







    instance (IsKind  (WrapKind :: a -> WrappedKind),IsKind  (WrapKind :: b -> WrappedKind)) =>
     HasInfo (Compose :: (b -> *) -> (a -> b) -> a -> *) where
    {
        info = mkSimpleInfo $(iowitness[t|WrapType Compose|]) [];
    };

    instance HasInfo ConstraintWitness where
    {
        info = mkSimpleInfo $(iowitness[t|WrapType ConstraintWitness|]) [];
    };



{-
    matchWit :: Wit f -> Wit fa -> Maybe (EqualType fa (f a),Wit a);
    matchWit _wf SimpleWit = Nothing;
    matchWit wf
-}

-- forall a. Info a -> Maybe (Info b,b -> a)


    constraintMaybe :: Knowledge;
    constraintMaybe = knowDependent (\w -> case w of
    {
        ConsWit im (MkInfo (ka :: Kind_X (WrapKind :: kka -> WrappedKind)) (wa :: Wit_W (WrapType (t :: kka))) _) -> do
        {
            Refl <- testEquality (ka :: Kind_X (WrapKind :: kka -> WrappedKind)) (witKind :: Kind_X (WrapKind :: * -> WrappedKind));
            Refl <- testEquality im (info :: Info Maybe);
            return (wa :: Wit_W (WrapType (t' :: *)),Just);
        };
        _ -> Nothing;
    });
{-
    constraintEqMaybe :: Knowledge;
    constraintEqMaybe = knowDependent (\w -> case w of
    {
        ConsWit wc (ConsWit (we wm wa -> case testEquality wf
        _ -> Nothing;
    });
-}




    matchConstraint :: forall (t :: *) (cons :: * -> Constraint).
     (HasInfo cons) => Info t -> Maybe (ConstraintWitness (cons t));
    matchConstraint = matchConstraint' matchProperty where
    {
        -- see http://hackage.haskell.org/trac/ghc/ticket/7527
        matchConstraint' :: forall (t' :: *) (cons' :: * -> Constraint).
            -- (Property (ConstraintFact cons)) =>
            (forall wt. Info_W wt -> Maybe (ConstraintFact cons' wt)) ->
             Info t' -> Maybe (ConstraintWitness (cons' t'));
        matchConstraint' matchProperty' a = do
        {
            -- MkWrappedProperty :: KindProperty (WrapKind :: k -> WrappedKind) (WrapType t) <- matchProperty a;
            MkWrapArgType (MkCompose cw) <- matchProperty' (a :: Info t');
            return cw;
        };
    };

{-
    instance (HasInfo (cons :: k -> Constraint),IsKind  (WrapKind :: k -> WrappedKind)) => Property (ConstraintFact cons) where
    {
        matchProperty = matchProperty_Fact;
    };

    instance (HasInfo cons,IsKind  (WrapKind :: k -> WrappedKind)) => Fact (ConstraintFact (cons :: k -> Constraint)) where
    {
        factInfoXW = MkInfoXW info;
    };
-}
    type Eq_Inst a = ConstraintFact Eq (WrapType a);
    instance HasInfo Eq where
    {
        info = mkSimpleInfo $(iowitness[t|WrapType Eq|]) [];
    };

    type FunctorOne_Inst a = ConstraintFact FunctorOne (WrapType a);
    instance HasInfo FunctorOne where
    {
        info = mkSimpleInfo $(iowitness[t|WrapType FunctorOne|]) [];
    };

    type HasNewValue_Inst a = ConstraintFact HasNewValue (WrapType a);
    instance HasInfo HasNewValue where
    {
        info = mkSimpleInfo $(iowitness[t|WrapType HasNewValue|]) [];
    };

    instance (HasInfo f,HasInfo a) => HasInfo (f a) where
    {
        info = applyInfo info info;
    };


    instance HasInfo Maybe where
    {
        info = mkSimpleInfo $(iowitness[t|WrapType Maybe|])
        [
            -- instance HasNewValue (Maybe a)
            mkFacts1 (\(a :: Info_W wt0) -> do
            {
                -- MkInfo _ _ _ <- return a;
                MkWrappedProperty :: KindProperty (WrapKind :: * -> WrappedKind) wt0 <- matchProperty a;
                return (MkWrapArgType (MkCompose (MkConstraintWitness :: ConstraintWitness (HasNewValue (Maybe t)))));
            }
            :: Maybe (ConstraintFact HasNewValue (WrapApply (WrapType Maybe) wt0))
            ),

            -- instance (Eq a) => Eq (Maybe a)
            mkFacts (MkFactS_U (\(a :: Info_W wt0) -> MkFactZ_U (do
            {
                MkWrappedProperty :: KindProperty (WrapKind :: * -> WrappedKind) wt0 <- matchProperty a;
                MkConstraintWitness :: ConstraintWitness (Eq t) <- matchConstraint a;
                return (MkWrapArgType (MkCompose MkConstraintWitness));
            }))
            :: Fact_U (ConstraintFact Eq) (WrapType Maybe)
            ),

            -- instance FunctorOne Maybe
            mkFacts (MkFactZ (do
            {
                return (MkWrapArgType (MkCompose MkConstraintWitness));
            })
            :: FactZ (ConstraintFact FunctorOne) (WrapType Maybe)
            )
        ];
    };

    instance HasInfo [] where
    {
        info = mkSimpleInfo $(iowitness[t| WrapType [] |])
        [
            -- instance () => HasNewValue ([] a)
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                MkWrappedProperty :: KindProperty (WrapKind :: * -> WrappedKind) wtt <- matchProperty a;
                return (MkWrapArgType (MkCompose MkConstraintWitness));
            }))
            :: FactS FactZ (ConstraintFact HasNewValue) (WrapType [])
            ),

            -- instance (Eq a) => Eq ([] a)
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                MkWrapArgType (MkCompose MkConstraintWitness) <- matchProp (Type :: Type (ConstraintFact Eq)) a;
                return (MkWrapArgType (MkCompose MkConstraintWitness));
            }))
            :: FactS FactZ (ConstraintFact Eq) (WrapType [])
            )
        ];
    };

    instance HasInfo () where
    {
        info = mkSimpleInfo $(iowitness[t| WrapType () |])
        [
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: HasNewValue_Inst ())),
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: Eq_Inst ()))
        ];
    };

    instance HasInfo Bool where
    {
        info = mkSimpleInfo $(iowitness[t| WrapType Bool |])
        [
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: HasNewValue_Inst Bool)),
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: Eq_Inst Bool))
        ];
    };

    instance HasInfo Word8 where
    {
        info = mkSimpleInfo $(iowitness[t| WrapType Word8 |])
        [
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: HasNewValue_Inst Word8)),
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: Eq_Inst Word8))
        ];
    };

    instance HasInfo Char where
    {
        info = mkSimpleInfo $(iowitness[t| WrapType Char |])
        [
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: HasNewValue_Inst Char)),
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: Eq_Inst Char))
        ];
    };

    instance HasInfo Int where
    {
        info = mkSimpleInfo $(iowitness[t| WrapType Int |])
        [
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: HasNewValue_Inst Int)),
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: Eq_Inst Int))
        ];
    };

    instance HasInfo ByteString where
    {
        info = mkSimpleInfo $(iowitness[t| WrapType ByteString |])
        [
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: HasNewValue_Inst ByteString)),
            mkFacts0 (return (MkWrapArgType (MkCompose MkConstraintWitness) :: Eq_Inst ByteString))
        ];
    };

    instance HasInfo (->) where
    {
        info = mkSimpleInfo $(iowitness[t| WrapType (->) |])
        [
        ];
    };

    instance HasInfo Result where
    {
        info = mkSimpleInfo $(iowitness[t| WrapType Result |])
        [
            -- instance (HasNewValue a1) => HasNewValue (Result a0 a1)
            mkFacts (MkFactS (\a0 -> MkFactS (\a1 -> MkFactZ (do
            {
                MkWrappedProperty :: KindProperty (WrapKind :: * -> WrappedKind) wtt <- matchProperty a0;
                MkWrapArgType (MkCompose MkConstraintWitness) <- matchProp (Type :: Type (ConstraintFact HasNewValue)) a1;
                return (MkWrapArgType (MkCompose MkConstraintWitness));
            })))
            :: FactS (FactS FactZ) (ConstraintFact HasNewValue) (WrapType Result)
            ),

            -- mkFacts1 (\_ -> return FunctorOne_Inst)
            mkFacts (MkFactS (\a0 -> MkFactZ (do
            {
                MkWrappedProperty :: KindProperty (WrapKind :: * -> WrappedKind) wtt <- matchProperty a0;
                return (MkWrapArgType (MkCompose MkConstraintWitness));
            }))
            :: FactS FactZ (ConstraintFact FunctorOne) (WrapType Result)
            )
        ];
    };

    instance HasInfo Any where
    {
        info = mkSimpleInfo $(iowitness[t| WrapType Any |])
        [
        ];
    };
}
