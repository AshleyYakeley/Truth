{-# OPTIONS -fno-warn-unused-binds -fno-warn-orphans #-}
module Truth.TypeKT.HasInfo where
{
    import Truth.TypeKT.Construct;
    import Truth.TypeKT.Type;
    import Truth.TypeKT.Kind;
    import Truth.TypeKT.TH;
    --import Truth.TypeKT.Basic;
    import qualified Language.Haskell.TH as TH;
    import Data.OpenWitness;
    import Data.Witness;
    import Data.List;
--    import Data.Maybe;
--    import Data.Bool;
--    import Data.Int;
--    import Data.Eq;
    import Control.Monad;
    import Prelude(Num(..),fromInteger);

    class HasInfo a where
    {
        info :: Info a;
    };

    instance (HasInfo a) => Property (EqualType a) where
    {
        matchProperty = matchWitness info;
    };

    $(fmap concat (forM supportedKinds (\k -> case k of
        {
            kpq@(TH.ArrowK kp kq) -> let
            {
                tp = kindTypeQ kp;
                tq = kindTypeQ kq;
                tpq = kindTypeQ kpq;
            } in
            [d|
                instance (HasInfo ($(tpq) f),HasInfo ($(tp) a)) => HasInfo ($(tq) (f a)) where
                {
                    info = applyInfo (info :: Info ($(tpq) f)) (info :: Info ($(tp) a));
                }
            |];
            _ -> return [];
        }
    )));


    factInstances :: TH.TypeQ -> TH.Q [TH.Dec];
    factInstances tq =
    {-
    [d|
        instance Property $(tq) where
        {
            matchProperty = matchProperty_Fact;
        };

        instance Fact $(tq) where
        {
            witFact = $(iowitness[t| $(tq) () |]);
        };
    |];
    -}
    sequence
    [
        TH.instanceD (return []) (TH.appT (TH.conT ''Property) tq)
         [TH.valD (TH.varP 'matchProperty) (TH.normalB (TH.varE 'matchProperty_Fact)) []],
        TH.instanceD (return []) (TH.appT (TH.conT ''Fact) tq)
         [TH.valD (TH.varP 'witFact) (TH.normalB (iowitness (TH.appT tq (TH.tupleT 0)))) []]
    ];


    --infoConstructed :: Info f -> Info a -> Info (TypeConstructed f a);


{- TODO
    instanceFacts :: TH.Cxt -> TH.Name -> [TH.Type] -> TH.ExpQ;
    instanceFacts _cxt cls _args = let
    {
        instWitTypeName :: TH.Name;
        instWitTypeName = TH.mkName ((TH.nameBase cls) ++ "_Inst");

        instWitType :: TH.TypeQ;
        instWitType = TH.conT instWitTypeName;

        instWitCons :: TH.Name;
        instWitCons = instWitTypeName;

        retWit :: TH.ExpQ;
        retWit = TH.appE (TH.varE 'return) (TH.varE instWitCons);

        matchKind :: TH.Kind -> TH.Name -> TH.ExpQ -> TH.ExpQ;
        matchKind k varname body = TH.caseE (TH.varE varname)
        [
            TH.match (TH.conP 'MkInfo [TH.conP (kindConsName k) [],TH.wildP,TH.wildP]) (TH.normalB body) [],
            TH.match TH.wildP (TH.normalB (TH.conE 'Nothing)) []
        ];

        argType :: TH.Type;
        argType = undefined';

        argKindQ :: TH.Q TH.Kind;
        argKindQ = typeKind argType;

        argBoxedType :: TH.TypeQ;
        argBoxedType = do
        {
            argKind <- argKindQ;
            TH.appT (TH.conT (kindTypeName argKind)) (return argType);
        };

        factsNType :: Int -> TH.TypeQ;
        factsNType 0 = TH.conT ''FactZ;
        factsNType n = TH.appT (TH.conT ''FactS) (factsNType (n - 1));

        ifType :: TH.TypeQ;
        ifType = TH.appT (TH.appT (factsNType 1) instWitType) argBoxedType;
    } in
    undefined' matchKind retWit ifType;

        undefined' :: a;
        undefined' = undefined';

    instanceFacts' :: TH.Cxt -> TH.Type -> TH.ExpQ;
    instanceFacts' cxt ctype = do
    {
        (name,ts) <- getAppl ctype;
        instanceFacts cxt name ts;
    } where
    {
        getAppl :: TH.Type -> TH.Q (TH.Name,[TH.Type]);
        getAppl (TH.ConT name) = return (name,[]);
        getAppl (TH.AppT t1 t2) = do
        {
            (name,ts) <- getAppl t1;
            return (name,ts ++ [t2]);
        };
        getAppl _ = do
        {
            TH.report True "bad instance type";
            fail "bad instance type";
        };
    };

    instancesFacts :: [TH.Dec] -> [TH.ExpQ];
    instancesFacts [] = [];
    instancesFacts (TH.InstanceD cxt ctype _:ds) = (instanceFacts' cxt ctype):(instancesFacts ds);
    instancesFacts (_:ds) = instancesFacts ds;
-}

}
