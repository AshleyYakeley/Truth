{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-unused-binds -w #-}
module Truth.TypeKT.Info
(
    module Truth.TypeKT.Info,
    Monoid(..)
) where
{
    import Truth.TypeKT.Type;
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;
    import Control.Monad;
	import Data.Monoid;
	import Control.Category;

    import Truth.TypeKT.TH;
    import Data.List;
    import Data.Char;
    import Data.Bool;
    import qualified Language.Haskell.TH as TH;
    import Language.Haskell.TH hiding (Info,Type,Kind);


    data Info t where
    {
        MkInfo :: forall t. (HasKind t) => Wit t -> Facts t -> Info t;
    };

    instance SimpleWitness Info where
    {
        matchWitness (MkInfo wa _) (MkInfo wb _) = matchWitness wa wb;
    };

    data Wit t where
    {
        SimpleWit :: forall t. IOWitness t -> Wit t;
        ConsWit :: forall f a. (ConstructType f a) => Info f -> Info a -> Wit (TypeConstructed f a);
    };

    instance SimpleWitness Wit where
    {
        matchWitness (SimpleWit iowa) (SimpleWit iowb) = matchWitness iowa iowb;
        matchWitness (ConsWit ica iaa) (ConsWit icb iab) = do
        {
            MkEqualType <- matchWitness ica icb;
            MkEqualType <- matchWitness iaa iab;
            return MkEqualType;
        };
        matchWitness _ _ = Nothing;
    };

    data Facts t = MkFacts
    {
        infoFact :: forall fact. (Fact fact) => Maybe (fact t),
        deriveFacts :: forall x. Info x -> Facts (TypeConstructed t x)
    };

    instance Monoid (Facts t) where
    {
        mempty = MkFacts Nothing (\_ -> mempty);
        mappend (MkFacts f1 d1) (MkFacts f2 d2) = MkFacts
         (mplus f1 f2)
         (\info -> mappend (d1 info) (d2 info));
    };

    mkSimpleInfo :: forall t. (HasKind t) => IOWitness t -> [Facts t] -> Info t;
    mkSimpleInfo wit facts = MkInfo (SimpleWit wit) (mconcat facts);

    class Property prop where
    {
        matchProperty :: forall t. Info t -> Maybe (prop t);
    };

    matchProp :: (Property prop) => Type (prop ()) -> Info t -> Maybe (prop t);
    matchProp _ = matchProperty;

{-
$(fmap concat (forM supportedKinds (\k -> do
    {
        [d|
            instance Property Kind_T where
            {
                matchProperty (MkInfo Kind_T _ _) = Just Kind_T;
                matchProperty _ = Nothing;
            };
        |]
    }))
);
-}
    class (Property fact) => Fact fact where
    {
        witFact :: IOWitness (fact ());
    };

    matchProperty_Fact :: (Fact prop) => Info t -> Maybe (prop t);
    matchProperty_Fact (MkInfo _ info) = infoFact info;

    newtype FactZ prop a = MkFactZ (Maybe (prop a));

    newtype FactS ff (prop :: * -> *) a = MkFactS (forall i. Info i -> ff prop (TypeConstructed a i));

    class FactChecker ff where
    {
        mkFacts :: forall fact a. (Fact fact) => ff fact a -> Facts a;
    };

    mkFacts0 :: (Fact fact) => Maybe (fact a) -> Facts a;
    mkFacts0 f = mkFacts (MkFactZ f);

    mkFacts1 :: (Fact fact) => (forall t0. Info t0 -> Maybe (fact (TypeConstructed a t0))) -> Facts a;
    mkFacts1 f = mkFacts (MkFactS (\a0 -> MkFactZ (f a0)));

    mkFacts2 :: (Fact fact) => (forall t0 t1. Info t0 -> Info t1 -> Maybe (fact (TypeConstructed (TypeConstructed a t0) t1))) -> Facts a;
    mkFacts2 f = mkFacts (MkFactS (\a0 -> MkFactS (\a1 -> MkFactZ (f a0 a1))));

    ffid :: forall fact t. (Fact fact) => (IOWitness (fact ()) -> Maybe (fact t)) -> Maybe (fact t);
    ffid mft = mft witFact;

    instance FactChecker FactZ where
    {
        mkFacts (MkFactZ (f :: Maybe (fact a))) = MkFacts (ffid (\wit -> do
        {
            MkEqualType <- matchWitness wit (witFact :: IOWitness (fact ()));
            f;
        })) (\_ -> mempty);
    };

    instance (FactChecker ff) => FactChecker (FactS ff) where
    {
        mkFacts (MkFactS f) = MkFacts Nothing (\i -> mkFacts (f i));
    };

    matchPropertyKind :: forall prop t. (IsKind prop) => Type (prop ()) -> Info t -> Maybe (prop t);
    matchPropertyKind _ (MkInfo _ _) = do
    {
        MkEqualType <- matchWitness (witKind :: IOWitness (TypeKind t ())) (witKind :: IOWitness (prop ()));
        return typeKind;
    };

$(fmap concat (forM supportedKinds (\k -> let
    {
        tkType = conT (mkName ("Kind_" ++ (kindCode k)));
    } in
    [d|
        instance Property $(tkType) where
        {
            matchProperty = matchPropertyKind $(type1 tkType);
        };
    |]))
);


{-
    $(fmap concat (forM supportedKinds (\k -> let
        {
            t = kindTypeQ k;
        } in let
        {
            funName = mkName ("matchKind_" ++ (kindCode k));
            funcType = [t| forall t r. Info t -> (forall a. (t ~ $(t) a) => Info ($(t) a) -> Maybe r) -> Maybe r |];
            sigDecl = TH.sigD funName funcType;
            sigDefn = do
            {
                tvar <- newName "t";
                fvar <- newName "f";
                TH.funD funName
                [
                TH.clause
                  [asP tvar (conP 'MkInfo [conP (kindConsName k) [],wildP,wildP]),varP fvar]
                  (normalB (appE (varE fvar) (varE tvar)))
                  []
                ,
                TH.clause [wildP,wildP] (normalB [|Nothing|]) []
                ];
            };
        } in sequence [sigDecl,sigDefn]
        {-
        [d|
        matchKind_T :: forall t r. Info t -> (forall a. (t ~ $(t) a) => Info ($(t) a) -> Maybe r) -> Maybe r;
        matchKind_T t@(MkInfo Kind_T _ _) f = f t;
        matchKind_T _ _ = Nothing;
        |]
        -}
    )));
-}

}

